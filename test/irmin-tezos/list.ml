
(*

605525   │ Jan  8 10:56:11.318 - validator.peer:   Error:
605526   │ Jan  8 10:56:11.318 - validator.peer:     {"Direct":["CoW7PhFfoc1FC9RCiFEbqDZjNA3XMnwFE5pCSpUvwQGWXU3nj8df",72051238680,319]}: unknown key

This is a key that points to an object at offset 72051238680.

{"dict_end_poff":3963935,"appendable_chunk_poff":613728839,"upgraded_from_v3_to_v4":true,"checksum":3602450451,"chunk_start_idx":106,"chunk_num":5,
"status":{
"Gced":{"suffix_start_offset":73830523438,
"generation":112,
"latest_gc_target_offset":73830523312,
"suffix_dead_bytes":272641}}}

But according to the control file, the start of the suffix is at offset
73830523438 and the latest gc target offset is 73830523312. The offset of the
missing key is less than both of these so this key is pointing to a commit that
has been garbage collected. How can this happen?

When attempting to read the offset and length from the prefix:
Fatal error: exception Pack_error: {"Invalid_read_of_gced_object":"offset 72051238680 is supposed to be contained in chunk (off=72051238554,poff=142144240,len=126) but starts after chunk"}

*)


open Lwt.Syntax
module Store = Irmin_tezos.Store

let main root _commit_hash _raw_path =
  Format.printf "\nLoading store from %s\n" root;
  let config =
    Irmin_pack.config ~readonly:true ~fresh:false
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal root
  in
  let* repo = Store.Repo.v config in

  Format.printf "\nControl File\n";
  let* _ = Store.debug_pp repo in

  Format.printf "\nLookup Offset\n";
  (* let offset, len = (73830523312, 319) in *)
  (* let offsets = *)
  (*   [ *)
  (*     (\* (72051242639, 246); *\) *)
  (*     (\* (73830523312, 126); *\) *)
  (*     (\* gc target *\) *)
  (*     (\* ( 72051238554, 126 ); *\) *)
  (*     (\* ( 72051242639, 246 ); *\) *)
  (*     (\* ( 72051250696, 138 ); *\) *)
  (*     (\* ( 73830250672, 272766 ) *\) *)
  (*     (\* ( 72051238680, 126 ); *\) *)
  (*     (\* (78713094710, 129); *\) *)
  (*   ] *)
  (* in *)

  (* let () = *)
  (*   List.iter *)
  (*     (fun (offset, len) -> *)
  (*       Store.lookup_offset repo (Optint.Int63.of_int offset) len) *)
  (*     offsets *)
  (* in *)

  (* commit we think contains inode hash *)
  let h = "CoUheNNbWzkqHBMdNaHGRCKfDQdLs8coVNudnwLsR8RhVdRWMWEm" in
  let inode_hash = "CoW7PhFfoc1FC9RCiFEbqDZjNA3XMnwFE5pCSpUvwQGWXU3nj8df" in

  (* let hash = Irmin.Type.(of_string Store.Hash.t h) |> Result.get_ok in *)
  (* let x = Irmin.Type.(unstage (encode_bin Store.Hash.t)) in *)
  (* let d = Irmin.Type.(unstage (decode_bin Store.Hash.t)) in *)
  (* let buf = Buffer.create 0 in *)
  (* x hash (fun s -> Buffer.add_string buf s); *)
  (* let to_bin_string = Irmin.Type.(unstage (to_bin_string Store.Hash.t)) in *)
  (* let s = to_bin_string hash in *)
  (* let l = ref 0 in *)
  (* let decoded_h = d (Buffer.contents buf) l in *)

  (* Fmt.pr "decoded = %s\n" Irmin.Type.(to_string Store.Hash.t decoded_h); *)

  (* Fmt.pr "%s\n" s; *)
  (* Fmt.pr "%s\n" (Buffer.contents buf); *)

  (* Fmt.pr "%s\n" (Hex.show (Hex.of_string s)); *)
  (* Fmt.pr "%s\n" (Hex.show (Hex.of_string @@ Buffer.contents buf)); *)

  (* let () = Store.lookup_offset repo (Optint.Int63.of_int 72051238680) 319 in *)
  let commits =
    [
      h
      (* "CoVjgRNK8bmYqPkSf4XbnyNJyNLrLHTvTz5Qa6mmNzUyvegqiznQ"; *)
      (* "CoWYzgUAiGQHvT9F55D5AueNLh6DU3Ec5SLeAdokTZhJW1XPiso4"; *)
      (* "CoVXLuDSPcSsHsj43rpdvyhiLmUTmLKqUg4C5q2Z4WRqT8z4VzAJ"; *)
      (* "CoUnaYV7CnLHbPic2UvyxHUeGmUnEvzLwMaHowEzmC9shmhhdihQ"; *)
      (* "CoVL7tk1YwTNm7av1yRAozGugiGaq1ZuVxmoxQTJAdJPHATvzgg7"; *);
    ]
  in
  Format.printf "\nChecking commits...\n";

  let* () =
    commits
    |> Lwt_list.iter_s @@ fun commit_hash ->
       Format.printf "Commit: %s\n" commit_hash;
       let hash =
         Irmin.Type.(of_string Store.Hash.t commit_hash) |> Result.get_ok
       in
       let* commit = Store.Commit.of_hash repo hash in
       let commit = Option.get commit in
       Format.printf "%a\n" Irmin.Type.(pp (Store.commit_t repo)) commit;
       let parents = Store.Commit.parents commit in
       let () = (match parents with
       | [ k ] ->
           Format.printf "-> Parent: %a\n" Irmin.Type.(pp Store.commit_key_t) k
       | _ -> ()) in
       let commit_key = Store.Commit.key commit in
       (*  inode key {"Direct":["CoWHvU98bo5HMC3VuKWs5mGP5Nw1X8sDHc1jxdTE2Mz3ZtD59qLk",31295677841,59]} *)
       let hash =
         Irmin.Type.(of_string Store.Hash.t inode_hash) |> Result.get_ok
       in
       Format.printf "\nIterate\n";
       Store.find_hash ~start_at:commit_key ~hash repo
  in

  (* let* heads = Store.Repo.heads repo in *)
  (* Format.printf "\nHead Commit Hashes\n"; *)
  (* (heads |> List.iter @@ fun h -> Format.printf "%a\n" Store.Commit.pp_hash h); *)

  (* let* branches = Store.Branch.list repo in *)
  (* Format.printf "\nBranches\n"; *)
  (* branches |> List.iter @@ Format.printf "%s\n"; *)

  (* Format.printf "\n\nCommit %s\n" commit_hash; *)
  (* let hash = Irmin.Type.(of_string Store.Hash.t commit_hash) |> Result.get_ok in *)
  (* let* commit = Store.Commit.of_hash repo hash in *)
  (* let* head_store = Store.of_commit (Option.get commit) in *)
  (* let* tree = Store.tree head_store in *)
  (* let path = *)
  (*   match String.split_on_char '/' raw_path with [ "" ] -> [] | x -> x *)
  (* in *)
  (* let* keys = Store.Tree.list ~offset:0 ~length:20 tree path in *)
  (* Format.printf "\nKeys\n"; *)
  (* (keys |> List.iter @@ fun (k, _) -> Format.printf "%s\n" k); *)
  Lwt.return_unit

let store = ref "node/data/context"
let commit_hash = ref "CoV8SQumiVU9saiu3FVNeDNewJaJH8yWdsGF3WLdsRr2P9S7MzCj"
let path = ref ""

let speclist =
  [
    ( "-store",
      Arg.Set_string store,
      "Store path. Defaults to node/data/context." );
    ( "-hash",
      Arg.Set_string commit_hash,
      "Commit hash. Defaults to \
       CoWLnZaqauuQpm9ewRuUDEU4wWWnD9S9xzW1BQXJnDNSmcKxNf9r, the genesis \
       context hash." );
    ("-path", Arg.Set_string path, "Path to list keys. Defaults to empty.");
  ]

let () =
  Arg.parse speclist
    (fun _ -> ())
    "main.exe -store /path/to/store -hash CommitHash -path data/contracts/index";
  Lwt_main.run @@ main !store !commit_hash !path
