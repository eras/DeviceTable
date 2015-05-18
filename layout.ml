open Util

module P = Containers_misc.PrintBox

(* Given a matrix of values, for each column find the maximum value
   and put them in order to result list *)
let max_widths xs =
  transpose xs
  |> List.map (List.fold_left max 0)

let box_width box =
  (P.Box.size box).P.x
    
let box_height box =
  (P.Box.size box).P.y

(* [rpad n box] pads an object by adding n spaces to its right side *)
let rpad n box =
  let open Containers_misc.PrintBox in
  hlist ~bars:false [box; text (String.make n ' ')]

(* return a box padded by its right side so that it is at least
   required_width characters wide; if it's already large enough (or
   larger), return the original box *)
let adjust_box_width box required_width =
  let orig_width = (P.Box.size box).P.x in
  if orig_width >= required_width
  then box
  else rpad (required_width - orig_width) box

(* for a list of boxes and an equally long list of widths, adjuste
   each box width to match the required width *)
let adjust_box_row_widths boxes required_widths = List.map2 adjust_box_width boxes required_widths

(* for a list of boxes, adjust each box size so that their total width will be exactly max_widths *)
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

(* produce a vertical bar as tall as "height" *)
let vertical_bar height =
  CCList.range 1 height |> List.map (const (P.text "|")) |> P.vlist ~bars:false

(* determine the maximum height of all boxes in a list of boxes *)
let row_height row = row |> List.map box_height |> List.fold_left max 0

(* on a row put a vertical bar to the left side of each box *)
let row_frames row =
  let height = row_height row in
  let rec loop accu = function
    | [] -> List.rev (accu)
    | x::xs -> loop (x::vertical_bar height::accu) xs
  in
  loop [] row

(* generate +---+ -kind of objects for a pair of rows so that +s go to
   the places where even objects are (so it assumes each row has a bar
   character at every other slot, beginning from the first element
   being a bar). *)
let middle_bars a b =
  let offsets = function
    | None -> []
    | Some boxes -> List.map box_width boxes |> scan_left ( + ) 0
  in
  let rec loop ofs ofs_a ofs_b accu =
    match ofs_a, ofs_b with
    | [], [] -> List.rev accu
    | ofs_a::_, ofs_b::_ when ofs_a < ofs && ofs_b < ofs ->
      assert false
    | ofs_a::_::rest_a, ofs_b::_::rest_b when ofs_a = ofs && ofs_b = ofs ->
      loop (ofs + 1) rest_a rest_b (P.text "+"::accu)
    | ofs_a::_::rest_a, _ when ofs_a = ofs ->
      loop (ofs + 1) rest_a ofs_b (P.text "+"::accu)
    | _, ofs_b::_::rest_b when ofs_b = ofs ->
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

(* TODO: there is a problem width dealing with vertical bars..

   as they too are aligned by the adjust_total_width function.

   Either the functionality to avoid that should be added to the
   adjust_total_width function, or the contents should be mapped very
   soon into something like ([`Left | `Right | `Center | `NoExpand] *
   P.Box.t) list which would be more general.
*)
let horizontal_lists rows =
  (* add left side edges to the boxes *)
  let rows = List.map row_frames rows in
  (* for a list of boxes, get their widths *)
  let col_widths boxes = List.map (fun b -> (P.Box.size b).P.x) boxes in
  (* acquire box widths of each row; group them by the number of boxes
     in each row *)
  let row_widths = (rows |> List.map @@ fun boxes -> (List.length boxes, col_widths boxes)) |> group_by identity in
  (* the desired width is the maximum size of each column for each
     different-ary row. (ie. 3-box columns are considered completely
     separately from 4-box columns) *)
  let adjusted_widths = row_widths |> List.map (project2nd max_widths) in
  (* maximum width: the longest width taking the adjusted_widths
     adjustment into account *)
  let max_width = adjusted_widths |> List.map snd |> List.map (sum) |> reduce max in
  (* acquire new adjusted_widths by enlongening box widths so that
     each row class (classified by the number of boxes in them)
     becomes as long *)
  let adjusted_widths = adjusted_widths |> List.map (project2nd (adjust_total_width max_width)) in
  (* finally produce the adjusted boxes using adjust_widths *)
  let adjusted_rows =
    rows
    |> List.map @@ fun boxes ->
    let len = List.length boxes in
    let height = row_height boxes in
    let adjusted_widths = List.assoc len adjusted_widths in
    adjust_box_row_widths boxes adjusted_widths |>
    (* the right-most vertical bar was not taken into account
       previously (so it wouldn't be aligned); add it here *)
    append (vertical_bar height)
  in
  (* between each row (and before the first and after the last) add
     the +----+ -rows *)
  let adjusted_rows = add_middle_bars adjusted_rows in
  (* finally we have he layout*)
  let layout = P.vlist ~bars:false (adjusted_rows |> List.map @@ fun boxes ->
                                    (P.hlist ~bars:false boxes))
  in
  layout

let foo () =
  horizontal_lists
    [[P.text "moi\naz"; P.text "moi"];
     [P.text "moi"; P.text "moi2"];
     [P.text "hello"; P.text "hello"; P.text "hello"];
     [P.text "hello"; P.text "he"; P.text "hello"];
     [P.text "hello"; P.text "h"; P.text "AEAEAEAE"; P.text "hello"];
     [P.text "hello"; P.text "he"; P.text "ae"; P.text "hello"]]

