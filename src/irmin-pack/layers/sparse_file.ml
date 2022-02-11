(** Implementation of sparse files; see documentation in [FIXME sparse_file_doc.mld] *)

(* pending issues

NOTE if we write data at off1, and then rewrite some different data at off1, both lots of
data will be recorded in the sparse file, but only the second lot of data will be
accessible at off1; we could add runtime checking to detect this

NOTE also that if we write n bytes at off1, and then different data at off1+m, where m<n,
we record both lots of data in the sparse file, and each lot of data is accessible, even
though this sparse file does not correspond to any proper original file; again we could
add runtime checking to detect this
*)



open Util

module type SPARSE = sig

  type map

  (** The type of sparse files. *)
  type t

  val create: path:string -> t
  (** [create ~path] creates an empty sparse file; requires [path] is non-existent;
      creates directory [path] and files [path/data] and [path/map] *)

  (** [open_ro ~dir] opens the sparse file located in [dir] *)
  val open_ro : dir:string -> t

  (** [close t] closes the underlying fd; this does not ensure the map is saved; use
      [save_map] for that. *)
  val close: t -> unit

  (** [append_region t ~src ~src_off ~len ~virt_off] appends len bytes from fd [src] at
      offset [src_off], to the end of sparse file t.

      A new map entry [virt_off -> (real_off,len)] is added, where real_off was the real
      offset of the end of the sparse file before the copy took place.

      NOTE [len] must not be 0; no other validity checks are made on off and len. It is
      possible to add the same region multiple times for example, or add overlapping
      regions, etc. {b These usages should be avoided.} FIXME perhaps we should check
      explicitly for these kinds of problems?
  *)
  val append_region: t -> src:Pread.t -> src_off:int -> len:int -> virt_off:int -> unit

end

module Private_map = struct
  type map = (int * int) Int_map.t

  include Int_map
  let load fn = 
    let ints = Small_int_file_v1.load fn in
    let ok = List.length ints mod 3 = 0 in
    let _check_ints_size = 
      if not ok then 
        failwith (P.s "%s: file %s did not contain 3*n ints" __FILE__ fn)
    in
    assert(ok);
    (ints,empty) |> iter_k (fun ~k (ints,m) -> 
        match ints with 
        | [] -> m
        | voff::off::len::rest ->
          (* each set of 3 ints corresponds to voff (virtual offset), off and len *)
          let m = add voff (off,len) m in
          k (rest,m)
        | _ -> failwith "impossible")

  let save t fn = 
    let x = ref [] in
    t |> bindings |> List.iter (fun (voff,(off,len)) -> x:= voff::off::len::!x);
    Small_int_file_v1.save !x fn

end


module Private = struct
  open Private_map
  open struct
    module Map_ = Private_map
  end

  let data_fn = "data"
  let map_fn = "map"


  type nonrec map = map

  type t = { 
    dir         : string;
    fd          : Unix.file_descr; 
    mutable map : map; 
    readonly    : bool 
  }

  (* Construction functions ---- *)

  (* We create the file as fn.tmp and fn.map.tmp while constructing,
     then promote when finished *)
  let create ~path = 
    let ok = not (Sys.file_exists path) in
    assert(ok);
    mkdir ~path;
    let fd = Unix.(openfile Fn.(path / data_fn) [O_RDWR;O_CREAT;O_TRUNC] 0o660) in
    { dir=path; fd; map=Map_.empty; readonly=false }

  let open_ro ~dir = 
    assert(Sys.file_exists dir);
    assert(Sys.file_exists Fn.(dir / data_fn));
    assert(Sys.file_exists Fn.(dir / map_fn));
    let fd = Unix.(openfile Fn.(dir / data_fn) [O_RDONLY] 0) in
    let map = Map_.load Fn.(dir / map_fn) in
    { dir; fd; map; readonly=true }

  let close t = 
    (if not t.readonly then Map_.save t.map Fn.(t.dir / data_fn));
    Unix.close t.fd

  let map_add t ~virt_off ~real_off ~len = t.map <- Map_.add virt_off (real_off,len) t.map

  let append_region t ~(src:Pread.t) ~src_off ~len ~virt_off =
    assert(len > 0);
    (* read in small chunks, in case the region is very large *)
    let buf_sz = 5 * 4096 in
    let buf = Bytes.create buf_sz in 
    (* seek to end of sparse file *)
    let real_off = Unix.(lseek t.fd 0 SEEK_END) in
    let off = ref src_off in
    len |> iter_k (fun ~k len -> 
        match len > 0 with 
        | false -> ()
        | true -> 
          src.pread ~off ~len:(min buf_sz len) ~buf |> fun n' -> 
          assert(n' > 0);
          assert(Unix.write t.fd buf 0 n' = n');
          k (len - n'));
    (* add map entry *)
    map_add t ~virt_off ~real_off ~len;
    ()

end

include (Private : SPARSE)

module Test() = struct

  module Sparse = Private

  (* performance test *)
  let perf_test () =
    let elapsed () = Mtime_clock.elapsed () |> Mtime.Span.to_s in
    (* copy 100 bytes every 100 bytes from a huge file *)
    let fn = Filename.temp_file "" ".tmp" in
    Printf.printf "(time %f) Creating huge 1GB file %s\n%!" (elapsed()) fn;
    let large_file = 
      (* create *)
      assert(Sys.command (Filename.quote_command "touch" [fn]) = 0);
      (* make huge *)
      assert(Sys.command (Filename.quote_command "truncate" ["--size=1GB";fn]) = 0);
      (* open *)
      let fd = Unix.(openfile fn [O_RDONLY] 0) in
      fd
    in
    (* open sparse file *)
    let fn2 = Filename.temp_file "" ".tmp" in
    Printf.printf "(time %f) Opening sparse file %s\n%!" (elapsed()) fn2;
    let t = Sparse.create ~path:fn2 in
    (* now copy n bytes every delta bytes *)
    Printf.printf "(time %f) Copying to sparse file\n%!" (elapsed());
    let len,delta = 100,500 in
    let count = ref 0 in
    let src = Pread.{pread=File.pread large_file} in
    0 |> iter_k (fun ~k src_off -> 
        match src_off+len < 1_000_000_000 with
        | true -> 
          Sparse.append_region t ~src ~src_off ~len ~virt_off:src_off;
          incr count;
          k (src_off+delta)
        | false -> ());
    Printf.printf "(time %f) Finished; number of regions: %d\n%!" (elapsed()) !count;
    Printf.printf "(time %f) Closing sparse file\n%!" (elapsed());
    Sparse.close t;
    Printf.printf "(time %f) Finished\n%!" (elapsed());
    ()

  (* typical run: 

     dune exec test/test.exe
     (time 0.000106) Creating huge 1GB file /tmp/8f53c9.tmp
     (time 0.002654) Opening sparse file /tmp/e6de54.tmp
     (time 0.002677) Copying to sparse file
     (time 12.xxxxx) Finished; number of regions: 2000000
     (time 12.329643) Closing sparse file
     (time 12.589131) Finished

     ls -al /tmp/
     -rw-rw----  1 tom  tom  191M Jan 14 17:15 e6de54.tmp
     -rw-rw----  1 tom  tom   31M Jan 14 17:15 e6de54.tmp.map

  *)

end


