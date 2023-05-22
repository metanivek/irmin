(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Metrics = Irmin.Metrics

module Inode = struct
  type Metrics.origin += Inode_stats

  type field =
    | Inode_add
    | Inode_remove
    | Inode_of_seq
    | Inode_of_raw
    | Inode_rec_add
    | Inode_rec_remove
    | Inode_to_binv
    | Inode_decode_bin
    | Inode_encode_bin

  type t = {
    mutable inode_add : int;
    mutable inode_remove : int;
    mutable inode_of_seq : int;
    mutable inode_of_raw : int;
    mutable inode_rec_add : int;
    mutable inode_rec_remove : int;
    mutable inode_to_binv : int;
    mutable inode_decode_bin : int;
    mutable inode_encode_bin : int;
  }
  [@@deriving irmin]

  type stat = t Metrics.t

  let create_inode () =
    {
      inode_add = 0;
      inode_remove = 0;
      inode_of_seq = 0;
      inode_of_raw = 0;
      inode_rec_add = 0;
      inode_rec_remove = 0;
      inode_to_binv = 0;
      inode_decode_bin = 0;
      inode_encode_bin = 0;
    }

  let clear m =
    let v = Metrics.state m in
    v.inode_add <- 0;
    v.inode_remove <- 0;
    v.inode_of_seq <- 0;
    v.inode_of_raw <- 0;
    v.inode_rec_add <- 0;
    v.inode_rec_remove <- 0;
    v.inode_to_binv <- 0;
    v.inode_decode_bin <- 0;
    v.inode_encode_bin <- 0

  let init () =
    let initial_state = create_inode () in
    Metrics.v ~origin:Inode_stats ~name:"inode_metric" ~initial_state t

  let export m = Metrics.state m

  let update ~field pack =
    let f v =
      match field with
      | Inode_add -> v.inode_add <- succ v.inode_add
      | Inode_remove -> v.inode_remove <- succ v.inode_remove
      | Inode_of_seq -> v.inode_of_seq <- succ v.inode_of_seq
      | Inode_of_raw -> v.inode_of_raw <- succ v.inode_of_raw
      | Inode_rec_add -> v.inode_rec_add <- succ v.inode_rec_add
      | Inode_rec_remove -> v.inode_rec_remove <- succ v.inode_rec_remove
      | Inode_to_binv -> v.inode_to_binv <- succ v.inode_to_binv
      | Inode_decode_bin -> v.inode_decode_bin <- succ v.inode_decode_bin
      | Inode_encode_bin -> v.inode_encode_bin <- succ v.inode_encode_bin
    in
    let mut = Metrics.Mutate f in
    Metrics.update pack mut
end

type t = { inode : Inode.stat }
type inode_size_ratio = { total : float; count : int }

let default_inode_size_ratio = { total = 0.0; count = 0 }

type priv = { mutable inode_size_ratio : inode_size_ratio }

let t = { inode = Inode.init () }
let priv = { inode_size_ratio = default_inode_size_ratio }
let get () = t

let reset_stats () =
  Inode.clear t.inode;
  priv.inode_size_ratio <- default_inode_size_ratio

let incr_inode_add () = Inode.update ~field:Inode.Inode_add t.inode
let incr_inode_remove () = Inode.update ~field:Inode.Inode_remove t.inode
let incr_inode_of_seq () = Inode.update ~field:Inode.Inode_of_seq t.inode
let incr_inode_of_raw () = Inode.update ~field:Inode.Inode_of_raw t.inode
let incr_inode_rec_add () = Inode.update ~field:Inode.Inode_rec_add t.inode

let incr_inode_rec_remove () =
  Inode.update ~field:Inode.Inode_rec_remove t.inode

let incr_inode_to_binv () = Inode.update ~field:Inode.Inode_to_binv t.inode

let incr_inode_decode_bin () =
  Inode.update ~field:Inode.Inode_decode_bin t.inode

let incr_inode_encode_bin () =
  Inode.update ~field:Inode.Inode_encode_bin t.inode

let add_inode_size_ratio ratio =
  priv.inode_size_ratio <-
    {
      total = priv.inode_size_ratio.total +. ratio;
      count = priv.inode_size_ratio.count + 1;
    }

let get_avg_inode_size_ratio () =
  match priv.inode_size_ratio.count with
  | 0 -> 0.0
  | c -> priv.inode_size_ratio.total /. Float.of_int c
