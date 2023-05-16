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
  (** An LRU that support memory-bound capacity via configuration key
      [lru_max_memory]. Falls back to entry-based capacity via [lru_size]
      configuration key, if max memory is not configured. *)

  val create : Irmin.Backend.Conf.t -> t

  val add : t -> 'a key -> (unit -> int) -> 'a -> unit
  (** [add t key weight value] maps [value] with [weight] to [key] in [t]. *)

  val find : t -> 'a key -> 'a
  val mem : t -> 'a key -> bool
  val clear : t -> unit

  (* val iter : t -> < f : 'a. 'a key -> 'a -> unit > -> unit *)
  val drop : t -> int option
  val size : t -> int
end

module Make_single (Val : Pack_value.Persistent) : sig
  module Key : sig
    type _ t = Value : int63 -> Val.t t
  end

  type 'a key = 'a Key.t

  include S with type 'a key := 'a Key.t
end

module Make
    (Commit : Irmin_pack.Pack_value.S)
    (Inode : Irmin_pack.Pack_value.S)
    (Contents : Irmin_pack.Pack_value.S) : sig
  module Key : sig
    type _ t =
      | Commit : int63 -> Commit.t t
      | Inode : int63 -> Inode.t t
      | Contents : int63 -> Contents.t t
  end

  type 'a key = 'a Key.t

  include S with type 'a key := 'a Key.t
end
