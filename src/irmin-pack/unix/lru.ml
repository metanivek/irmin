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
  type t
  type key = int63
  type value

  val create : Irmin.Backend.Conf.t -> t
  val add : t -> key -> int -> value -> unit
  val find : t -> key -> value
  val mem : t -> key -> bool
  val clear : t -> unit
end

module Internal = Irmin.Backend.Lru.Make (struct
  include Int63

  let hash = Hashtbl.hash
end)

module Make (Val : Pack_value.Persistent) = struct
  type key = int63
  type value = Val.t
  (* type weighted_value = { v : value; weight : int } *)

  type t = {
    lru : value Internal.t;
    weight_limit : int option;
    mutable total_weight : int;
  }

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
    let lru = Internal.create lru_size in
    { lru; weight_limit; total_weight = 0 }

  (* let lru_enabled t = match t.weight_limit with None -> true | Some x -> x > 0 *)

  let add t k w v =
    ignore w;
    Internal.add t.lru k v
  (* if lru_enabled t = false then () *)
  (* else *)
  (*   let add t k v w = *)
  (*     (\* let n = { v; weight = w } in *\) *)
  (*     t.total_weight <- t.total_weight + w; *)
  (*     Internal.add t.lru k v *)
  (*   in *)
  (*   match t.weight_limit with *)
  (*   | None -> add t k v 0 *)
  (*   | Some limit -> *)
  (*       add t k v (w ()); *)
  (*       while t.total_weight > limit do *)
  (*         match Internal.drop t.lru with *)
  (*         | None -> t.total_weight <- 0 *)
  (*         | Some _ -> t.total_weight <- t.total_weight - 1 *)
  (*         (\* garbage obvi *\) *)
  (*       done *)

  let find { lru; _ } k = Internal.find lru k
  let mem { lru; _ } k = Internal.mem lru k
  let clear { lru; _ } = Internal.clear lru
end
