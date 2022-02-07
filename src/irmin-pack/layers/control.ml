(** The control file records which are the "current" implementation files (objects,
    suffix), as well as some metadata: generation, version, last flushed offset.


The control file uses an mmap. RO instances should check generation and resync if required.

NOTE 
*)

(** 

NOTE: structure of layers directory:

Typically the "generation" is some number 1234 say. Then we expect these files to exist:

- control (contains the generation number, and pointers to the rest of the data; RO
  instances should check the generation to see when it changes, and if so
  resync)
- objects.1234/ \{objects.data,objects.map\}
- suffix.1234/ \{suffix.data,suffix.offset\}
*)

open Util

module Private = struct

  let default_version = 2

  type t  = Int_mmap.t

  (** Metadata fields are identifed by their index in the array; use the predefined fields,
      don't create your own *)
  type field = int

  let generation_field : field = 1

  let version_field : field = 2

  let last_synced_offset_field : field = 3


  (** [length] is the number of metadata fields, and hence the size of the array *)
  let length = 3

  let set t index v = t.Int_mmap.arr.{ index } <- v

  let get t index = t.Int_mmap.arr.{ index }

  let create ~root ~name = 
    let ok = not (Sys.file_exists Fn.(root / name)) in
    assert(ok);
    let t = Int_mmap.create ~dir:root ~name ~sz:2 in
    set t version_field default_version;
    t

  let open_ ~root ~name = 
    let ok = Sys.file_exists Fn.(root / name) in
    assert(ok);
    let t = Int_mmap.open_ ~dir:root ~name ~sz:2 in
    t

  let close t = Int_mmap.close t

  (* NOTE it is believed that under Linux this implies that the mmap is flushed as if via
     msync FIXME perhaps use msync instead *)
  let fsync (t:t) = Unix.fsync t.fd

  let get_generation t = get t generation_field

  let suffix_name t = "suffix."^(get t generation_field |> string_of_int)

  let objects_name t = "objects."^(get t generation_field |> string_of_int)
end

include (Private : sig
  type t
  type field
  val generation_field : field
  val version_field : field
  val last_synced_offset_field : field
  val length : field
  val set : t -> field -> int -> unit
  val get : t -> field -> int
  val create : root:string -> name:string -> t
  val open_ : root:string -> name:string -> t
  val fsync : t -> unit
  val close : t -> unit
  val get_generation : t -> int
  (** Convenience; just [get t generation] *)
  val suffix_name : t -> string
  (** Default name for suffix subdir; "suffix.nnnn" where nnnn is the generation number *)
  val objects_name : t -> string
  (** Default name for objects subdir; "objects.nnnn" where nnnn is the generation number *)
end)