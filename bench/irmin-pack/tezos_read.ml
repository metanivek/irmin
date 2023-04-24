module Store = Irmin_tezos.Store

type cut_side = Above | Below
type cut = { side : cut_side; offset : Optint.Int63.t }

module OffsetCompare = struct
  open Optint.Int63

  let ( < ) a b = compare a b < 0
  let ( <= ) a b = compare a b <= 0
  let ( > ) a b = compare a b > 0
  let ( >= ) a b = compare a b >= 0
  let ( = ) = equal
end

let is_in_cut cut offset =
  match cut with
  | None -> true
  | Some cut -> (
      let open OffsetCompare in
      match cut.side with
      | Above -> offset >= cut.offset
      | Below -> offset < cut.offset)

let load_commits repo =
  let fm = Store.Internal.file_manager repo in
  let index = Store.Internal.File_manager.index fm in
  let commits = ref [] in
  let f key value =
    let offset, _, kind = value in
    match kind with
    | Irmin_pack.Pack_value.Kind.Commit_v1
    | Irmin_pack.Pack_value.Kind.Commit_v2 ->
        commits := List.cons (offset, key) !commits
    | _ -> ()
  in
  Store.Internal.File_manager.Index.iter f index;
  List.sort
    (fun (offset_a, _) (offset_b, _) -> Optint.Int63.compare offset_a offset_b)
    !commits

let read_entire_commit repo (offset, hash) =
  Fmt.epr "Load (%a, %s)... " Optint.Int63.pp offset
    Irmin.Type.(to_string Store.Hash.t hash);
  let open Lwt.Syntax in
  let* commit = Store.Commit.of_hash repo hash in
  match commit with
  | None ->
      Fmt.epr "Not Found!@.";
      Lwt.return_none
  | Some commit ->
      let c0 = Mtime_clock.counter () in
      let* () = Store.Tree.fold (Store.Commit.tree commit) () in
      let time = Mtime_clock.count c0 |> Mtime.Span.to_ms in
      Fmt.epr "Done in %fms@." time;
      Lwt.return_some time

let main root lower_root lru_size read_every high_to_low cut =
  let open Lwt.Syntax in
  let root =
    match root with None -> failwith "root not provided" | Some root -> root
  in
  let conf =
    Irmin_pack.config ~readonly:true ~fresh:false ~lru_size
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~lower_root root
  in
  let* repo = Store.Repo.v conf in
  let commits = load_commits repo in
  let sampled_commits = ref [] in
  List.iteri
    (fun i (offset, key) ->
      if i mod read_every = 0 && is_in_cut cut offset then
        sampled_commits := List.cons (offset, key) !sampled_commits
      else ())
    commits;
  let sampled_commits =
    if high_to_low = false then List.rev !sampled_commits else !sampled_commits
  in
  let* times = Lwt_list.map_s (read_entire_commit repo) sampled_commits in
  let actual_times = List.filter Option.is_some times |> List.map Option.get in
  let total = List.fold_left Float.add 0. actual_times in
  let count = List.length actual_times in
  Fmt.epr "Avg: %f\n" (total /. Int.to_float count);
  List.iter (function Some f -> Fmt.pr "%f\n" f | None -> Fmt.pr "-\n") times;
  Lwt.return_unit

let main_cmd root lower_root lru_size read_every high_to_low cut_offset cut_side
    =
  let cut =
    match cut_offset with
    | None -> None
    | Some offset ->
        Some { side = cut_side; offset = Optint.Int63.of_int offset }
  in
  (* TOOD: better way to integrate Lwt with Cmdliner? *)
  Lwt_main.run @@ main root lower_root lru_size read_every high_to_low cut

open Cmdliner

let root =
  let doc = Arg.info ~doc:"root path" [ "root" ] in
  Arg.(value @@ opt (some dir) None doc)

let lower_root =
  let doc = Arg.info ~doc:"lower root path" [ "lower-root" ] in
  Arg.(value @@ opt (some dir) None doc)

let lru_size =
  let doc = Arg.info ~doc:"LRU size" [ "lru-size" ] in
  Arg.(value @@ opt int 100_000 doc)

let read_every =
  let doc = Arg.info ~doc:"Read every N commits" [ "read-every" ] in
  Arg.(value @@ opt int 1000 doc)

let high_to_low =
  let doc =
    Arg.info ~doc:"Iterate from highest offset to lowest offset"
      [ "high-to-low" ]
  in
  Arg.(value @@ opt bool true doc)

let cut_offset =
  let doc = Arg.info ~doc:"Cut point for reading" [ "cut-offset" ] in
  Arg.(value @@ opt (some int) None doc)

let read_cut_side =
  let doc = Arg.info ~doc:"Which side of the cut to read" [ "cut-side" ] in
  Arg.(value @@ opt (enum [ ("Above", Above); ("Below", Below) ]) Above doc)

let main_term =
  Term.(
    const main_cmd
    $ root
    $ lower_root
    $ lru_size
    $ read_every
    $ high_to_low
    $ cut_offset
    $ read_cut_side)

let deprecated_info = (Term.info [@alert "-deprecated"])
let deprecated_exit = (Term.exit [@alert "-deprecated"])
let deprecated_eval = (Term.eval [@alert "-deprecated"])

let () =
  let man =
    [
      `S "DESCRIPTION";
      `P "Benchmark for sampling commits from a Tezos context store.";
    ]
  in
  let info =
    deprecated_info ~man ~doc:"Benchmarks for sampling Tezos context commits"
      "tezos_read"
  in
  deprecated_exit @@ deprecated_eval (main_term, info)
