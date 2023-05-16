(*
 * Copyright (c) 2023 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Import

module type S = sig
  type 'a key
  type t

  val create : Irmin.Backend.Conf.t -> t
  val add : t -> 'a key -> (unit -> int) -> 'a -> unit
  val find : t -> 'a key -> 'a
  val mem : t -> 'a key -> bool
  val clear : t -> unit

  (* val iter : t -> < f : 'a. 'a key -> 'a -> unit > -> unit *)
  val drop : t -> int option
  val size : t -> int
end

module type Key = sig
  type _ t

  val equal : _ t -> _ t -> bool
  val hash : _ t -> int

  type pair = Pair : 'a t * 'a -> pair

  val unpack : 'a t -> pair -> 'a
end

module Make_lru (K : Key) : S with type 'a key = 'a K.t = struct
  module Key = K

  type 'a key = 'a Key.t
  type k = Key : 'a Key.t -> k

  module Lru = Irmin.Backend.Lru.Make (struct
    type t = k

    let hash (Key k) = Key.hash k
    let equal (Key l) (Key r) = Key.equal l r
  end)

  type lru = (Key.pair * int) Lru.t
  type t = { lru : lru; weight_limit : int option; mutable total_weight : int }

  let calculate_weight_limit = function
    | `Byte b -> b
    | `Megabyte b -> 1_000_000 * b
    | `Gigabyte b -> 1_000_000_000 * b

  let create config =
    let lru_max_memory = Irmin_pack.Conf.lru_max_memory config in
    let lru_size, weight_limit =
      match lru_max_memory with
      | None -> (Irmin_pack.Conf.lru_size config, None)
      | Some b -> (-42, calculate_weight_limit b |> Option.some)
    in
    let lru = Lru.create lru_size in
    { lru; weight_limit; total_weight = 0 }

  let lru_enabled t = match t.weight_limit with None -> true | Some x -> x > 0
  let drop { lru; _ } = Lru.drop lru |> Option.map (fun (_, (_, w)) -> w)
  let size { lru; _ } = Lru.size lru

  let add t k w v =
    if lru_enabled t = false then ()
    else
      let add w =
        let v = (Key.Pair (k, v), w) in
        let k = Key k in
        t.total_weight <- t.total_weight + w;
        Lru.add t.lru k v
      in
      match t.weight_limit with
      | None -> add 0
      | Some limit ->
          add (w ());
          while t.total_weight > limit do
            match drop t with
            | None -> t.total_weight <- 0
            | Some weight -> t.total_weight <- t.total_weight - weight
          done

  let find { lru; _ } k = Key.unpack k (Lru.find lru (Key k) |> fst)
  let mem { lru; _ } k = Lru.mem lru (Key k)
  let clear { lru; _ } = Lru.clear lru
  (* let iter { lru; _ } f = Lru.iter lru (fun k wv -> f k (v wv)) *)
end

module Make_key
    (Commit : Irmin_pack.Pack_value.S)
    (Inode : Irmin_pack.Pack_value.S)
    (Contents : Irmin_pack.Pack_value.S) =
struct
  type _ t =
    | Commit : int63 -> Commit.t t
    | Inode : int63 -> Inode.t t
    | Contents : int63 -> Contents.t t

  let equal : type a b. a t -> b t -> bool =
   fun l r ->
    match (l, r) with
    | Commit x, Commit y -> Int63.equal x y
    | Inode x, Inode y -> Int63.equal x y
    | Contents x, Contents y -> Int63.equal x y
    | _ -> false

  let hash = Hashtbl.hash

  type pair = Pair : 'a t * 'a -> pair

  let unpack : type a. a t -> pair -> a =
   fun k p ->
    match (k, p) with
    | Commit _, Pair (Commit _, v) -> v
    | Inode _, Pair (Inode _, v) -> v
    | Contents _, Pair (Contents _, v) -> v
    | _ -> assert false
end

module Make_single (Val : Irmin_pack.Pack_value.S) = struct
  module Key = struct
    type 'a t = Value : int63 -> Val.t t

    let equal : type a b. a t -> b t -> bool =
     fun l r -> match (l, r) with Value x, Value y -> Int63.equal x y

    let hash = Hashtbl.hash

    type pair = Pair : 'a t * 'a -> pair

    let unpack : type a. a t -> pair -> a =
     fun k p -> match (k, p) with Value _, Pair (Value _, v) -> v
  end

  include Make_lru (Key)
end

module Make
    (Commit : Irmin_pack.Pack_value.S)
    (Inode : Irmin_pack.Pack_value.S)
    (Contents : Irmin_pack.Pack_value.S) =
struct
  module Key = Make_key (Commit) (Inode) (Contents)
  include Make_lru (Key)
end

(* module Internal = Irmin.Backend.Lru.Make (struct *)
(*   include Int63 *)

(*   let hash = Hashtbl.hash *)
(* end) *)

(* type 'a key = int63 * 'a *)

(* (\* type value = Irmin_pack.Pack_value.kinded *\) *)
(* type 'a weighted_value = { v : 'a; weight : int } *)

(* type _ t = { *)
(*   lru : _ weighted_value Internal.t; *)
(*   weight_limit : int option; *)
(*   mutable total_weight : int; *)
(* } *)

(* let calculate_weight_limit = function *)
(*   | `Byte b -> b *)
(*   | `Megabyte b -> 1_000_000 * b *)
(*   | `Gigabyte b -> 1_000_000_000 * b *)

(* let create config = *)
(*   let lru_max_memory = Irmin_pack.Conf.lru_max_memory config in *)
(*   let lru_size, weight_limit = *)
(*     match lru_max_memory with *)
(*     | None -> (Irmin_pack.Conf.lru_size config, None) *)
(*     | Some b -> (-42, calculate_weight_limit b |> Option.some) *)
(*   in *)
(*   let lru = Internal.create lru_size in *)
(*   { lru; weight_limit; total_weight = 0 } *)

(* let lru_enabled t = match t.weight_limit with None -> true | Some x -> x > 0 *)

(* let add t k w v = *)
(*   if lru_enabled t = false then () *)
(*   else *)
(*     let add t k v w = *)
(*       let n = { v; weight = w } in *)
(*       t.total_weight <- t.total_weight + w; *)
(*       (\* let c () = *\) *)
(*       (\*   let s = Internal.size t.lru in *\) *)
(*       (\*   Printf.eprintf "[LRU] V: %d, N: %d (%d)\n" (Mem.reachable_bytes v) *\) *)
(*       (\*     (Mem.reachable_bytes n) s *\) *)
(*       (\* in *\) *)
(*       (\* Mem.maxrss_delta ~c "LRU.add" @@ fun () -> Internal.add t.lru k n *\) *)
(*       Internal.add t.lru k n *)
(*     in *)
(*     match t.weight_limit with *)
(*     | None -> add t k v 0 *)
(*     | Some limit -> *)
(*         add t k v (w ()); *)
(*         while t.total_weight > limit do *)
(*           match Internal.drop t.lru with *)
(*           | None -> t.total_weight <- 0 *)
(*           | Some n -> t.total_weight <- t.total_weight - n.weight *)
(*         done *)

(* let v v = v.v *)
(* let find { lru; _ } k = Internal.find lru k |> v *)
(* let mem { lru; _ } k = Internal.mem lru k *)
(* let clear { lru; _ } = Internal.clear lru *)
(* let iter { lru; _ } f = Internal.iter lru (fun k wv -> f k (v wv)) *)
