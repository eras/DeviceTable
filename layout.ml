open Util

module P = Containers_misc.PrintBox

let max_widths xs =
  transpose xs
  |> List.map (List.fold_left max 0)

let box_width box =
  (P.Box.size box).P.x
    
let box_height box =
  (P.Box.size box).P.y

let rpad n box =
  let open Containers_misc.PrintBox in
  hlist ~bars:false [box; text (String.make n ' ')]

let adjust_box_width box required_width =
  let orig_width = (P.Box.size box).P.x in
  if orig_width >= required_width
  then box
  else rpad (required_width - orig_width) box

let adjust_box_row_widths boxes required_widths = List.map2 adjust_box_width boxes required_widths

let adjust_total_width max_width widths =
  let cur_width = sum widths in
  let difference = max_width - cur_width in
  if difference <= 0
  then widths
  else
    let num_widths = List.length widths in
    let diff_per_cell = difference / num_widths in
    let diff_remainder = difference mod num_widths in
    let rec loop accu = function
      | [] -> List.rev accu
      | last::[] -> loop (last + diff_per_cell + diff_remainder::accu) []
      | x::xs -> loop (x + diff_per_cell::accu) xs
    in
    loop [] widths

let vertical_bar height =
  CCList.range 1 height |> List.map (const (P.text "|")) |> P.vlist ~bars:false

let row_height row = row |> List.map box_height |> List.fold_left max 0

let row_frames row =
  let height = row_height row in
  let rec loop accu = function
    | [] -> List.rev (accu)
    | x::xs -> loop (x::vertical_bar height::accu) xs
  in
  loop [] row

let middle_bars a b =
  let offsets = function
    | None -> []
    | Some boxes -> List.map box_width boxes |> scan_left ( + ) 0
  in
  let rec loop ofs ofs_a ofs_b accu =
    match ofs_a, ofs_b with
    | [], [] -> List.rev accu
    | ofs_a::_::rest_a, ofs_b::_::rest_b when ofs_a == ofs && ofs_b == ofs ->
      loop (ofs + 1) rest_a rest_b (P.text "+"::accu)
    | ofs_a::_::rest_a, _ when ofs_a == ofs ->
      loop (ofs + 1) rest_a ofs_b (P.text "+"::accu)
    | _, ofs_b::_::rest_b when ofs_b == ofs ->
      loop (ofs + 1) ofs_a rest_b (P.text "+"::accu)
    | _ ->
      loop (ofs + 1) ofs_a ofs_b (P.text "-"::accu)
  in
  loop 0 (offsets a) (offsets b) []

let add_middle_bars boxes =
  let rec loop = function
    | [] -> []
    | x::[] -> [x] @ [middle_bars (Some x) None]
    | a::b::rest -> a::middle_bars (Some a) (Some b)::(loop (b::rest))
  in
  match boxes with
  | [] -> []
  | (x::_) as xs -> middle_bars None (Some x)::loop xs

let horizontal_lists rows =
  (* add left side edges to the boxes *)
  let rows = List.map row_frames rows in
  let col_widths boxes = List.map (fun b -> (P.Box.size b).P.x) boxes in
  let row_widths = (rows |> List.map @@ fun boxes -> (List.length boxes, col_widths boxes)) |> group_by identity in
  let adjusted_widths = row_widths |> List.map (project2nd max_widths) in
  let max_width = adjusted_widths |> List.map snd |> List.map (sum) |> reduce max in
  let adjusted_widths = adjusted_widths |> List.map (project2nd (adjust_total_width max_width)) in
  let adjusted_rows =
    rows
    |> List.map @@ fun boxes ->
    let len = List.length boxes in
    let height = row_height boxes in
    let adjusted_widths = List.assoc len adjusted_widths in
    adjust_box_row_widths boxes adjusted_widths |>
    append (vertical_bar height)
  in
  let adjusted_rows = add_middle_bars adjusted_rows in
  let layout = P.vlist ~bars:false (adjusted_rows |> List.map @@ fun boxes ->
                                    (P.hlist ~bars:false boxes))
  in
  P.output Pervasives.stdout layout;
  max_width

let foo () =
  horizontal_lists
    [[P.text "moi\naz"; P.text "moi"];
     [P.text "moi"; P.text "moi2"];
     [P.text "hello"; P.text "hello"; P.text "hello"];
     [P.text "hello"; P.text "he"; P.text "hello"];
     [P.text "hello"; P.text "h"; P.text "AEAEAEAE"; P.text "hello"];
     [P.text "hello"; P.text "he"; P.text "ae"; P.text "hello"]]

