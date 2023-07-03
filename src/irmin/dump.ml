module H = Hashtbl.Make (String)

let h = H.create 64

let open_ name path n =
  let ch = Out_channel.open_text path in
  H.add h name (ch, n)

let row arr =
  let n = Array.length arr in
  Array.fold_left
    (fun acc elem -> acc ^ "," ^ elem)
    arr.(0)
    (Array.sub arr 1 (n - 1))
  ^ "\n"

let make_row c n v =
  assert (c >= 0 && n > 0);
  assert (c < n);
  let arr = Array.init n (fun _ -> "") in
  arr.(c) <- v;
  row arr

let set_row name v =
  match H.find_opt h name with
  | None -> ()
  | Some (ch, n) ->
      let r =
        match v with
        | `Arr arr ->
            assert (Array.length arr = n);
            row arr
        | `Val (col, v) ->
            assert (col < n);
            make_row col n v
      in
      Out_channel.output_string ch r;
      Out_channel.flush ch

let set_arr name arr = set_row name (`Arr arr)
let set name col v = set_row name (`Val (col, v))
let set_int name col v = set name col (string_of_int v)
let set_float name col v = set name col (string_of_float v)
let add name col = set_int name col 1

let close () =
  H.iter
    (fun _ (ch, _) ->
      Out_channel.flush ch;
      Out_channel.close ch)
    h
