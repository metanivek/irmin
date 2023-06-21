module H = Hashtbl.Make (String)

let h = H.create 64

let open_ name path n =
  let ch = Out_channel.open_text path in
  H.add h name (ch, n)

let row c n v =
  assert (c >= 0 && n > 0);
  assert (c < n);
  let arr = Array.init n (fun _ -> "") in
  arr.(c) <- string_of_int v;
  Array.fold_left
    (fun acc elem -> acc ^ "," ^ elem)
    arr.(0)
    (Array.sub arr 1 (n - 1))
  ^ "\n"

let set name col v =
  match H.find_opt h name with
  | None -> ()
  | Some (ch, n) ->
      assert (col < n);
      let r = row col n v in
      Out_channel.output_string ch r

let add name col = set name col 1

let close () =
  H.iter
    (fun _ (ch, _) ->
      Out_channel.flush ch;
      Out_channel.close ch)
    h
