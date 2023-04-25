module Io = Irmin_pack_unix.Io.Unix
module Io_errors = Irmin_pack_unix.Io_errors.Make (Io)
module Upper_control = Irmin_pack_unix.Control_file.Upper (Io)

let ( let* ) = Result.bind

let main cf_path volume_num =
  let r =
    let tmp_path = cf_path ^ ".tmp" |> Option.some in
    let* cf = Upper_control.open_ ~path:cf_path ~tmp_path ~readonly:false in
    let payload =
      let pl = Upper_control.payload cf in
      match volume_num with None -> pl | Some v -> { pl with volume_num = v }
    in
    Upper_control.set_payload cf payload
  in
  r |> Io_errors.raise_if_error

open Cmdliner

let control_file =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"CONTROL_FILE_PATH" ~doc:"the path to the upper control file")

let volume_num =
  Arg.(
    value
    & opt (some int) None
    & info ["volume-num"] ~docv:"VOLUME_NUM" ~doc:"override new volume number")

let main_cmd =
  let doc = "in-place update of previous 3.7 mvp upper control files" in
  let info = Cmd.info "cfupdate" ~doc in
  Cmd.v info Term.(const main $ control_file $ volume_num)

let () = exit (Cmd.eval ~catch:false main_cmd)
