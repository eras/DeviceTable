let identity a = a

let const a b = a

let flip f a b = f b a

let sum = List.fold_left ( + ) 0

let project1st f (x, y) = (f x, y)

let project2nd f (x, y) = (x, f y)

let project_to_2nd f x = (x, f x)

let reduce f xs = List.fold_left f (List.hd xs) (List.tl xs)

let append x xs = xs @ [x]

let scan_left f v0 xs =
  List.rev @@
  List.fold_left
    (fun accu x ->
       let v = List.hd accu in
       f v x::accu
    )
    [v0]
    xs

let group_by f xs =
  let h = Hashtbl.create 10 in
  List.iter (fun x ->
      let v, v' = f x in
      try Hashtbl.replace h v (v'::Hashtbl.find h v)
      with Not_found -> Hashtbl.add h v [v']) xs;
  Hashtbl.fold (fun k vs ks -> (k, vs)::ks) h []

(* picked from http://stackoverflow.com/questions/3989776/transpose-of-a-list-of-lists :( *)
let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
      (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

module Op = struct
  let (^/) a b = a ^ "/" ^ b
end

let list_files dir =
  let dir = Unix.opendir dir in
  let rec get_files aux =
    match try Some (Unix.readdir dir)
          with End_of_file -> None 
    with
    | Some ("." | "..") -> get_files aux
    | Some file -> get_files (file::aux)
    | None -> List.rev aux
  in
  let files = get_files [] in
  Unix.closedir dir;
  files

let list_files_with_base dir =
  List.map (fun s -> dir ^ "/" ^ s) (list_files dir)

let pmatch ~pat = Re.execp (Re_pcre.re pat |> Re.compile)

let extract ~pat str = Re.get_all (Re.exec (Re_pcre.re pat |> Re.compile) str)

let contents file = CCIO.with_in file CCIO.read_all 

let first_line file =
  contents file |> Re.replace (Re_pcre.re ~flags:[`MULTILINE] "\n.*" |> Re.compile) ~f:(const "")

let drop_while f xs =
  let rec loop xs =
    match xs with
    | x::xs when f x -> loop xs
    | xs -> xs
  in
  loop xs

let trim_begin = Re.replace (Re_pcre.re "^ *" |> Re.compile) ~f:(const "")

let expand_second xs =
  xs
  |> List.map (fun (x, xs) -> xs |> List.map (fun y -> (x, y)))
  |> List.concat

let flip_list xs = List.map (fun (a, b) -> (b, a)) xs

let find_opt p xs =
  try Some (List.find p xs)
  with Not_found -> None

let device_of_stat s = Types.Device (s.Unix.st_rdev lsr 8, s.Unix.st_rdev land 0xff)

let block_devices () =
  let base = "/dev" in
  list_files base
  |> CCList.map (project_to_2nd (fun s -> base ^ "/" ^ s))
  |> CCList.map (project2nd Unix.stat)
  |> CCList.filter (fun (_, s) -> s.Unix.st_kind = Unix.S_BLK)
  |> CCList.map (project2nd @@ device_of_stat)

let name_of_block_device =
  let names = flip_list @@ block_devices () in
  fun ((Types.Device (major, minor)) as device) ->
    try List.assoc device names
    with Not_found -> Printf.sprintf "%d:%d" major minor

let read_dev dev_file =
  first_line dev_file |> Re.split (Re_pcre.re ":" |> Re.compile) |> function
  | major::minor::_ -> Types.Device (int_of_string major, int_of_string minor)
  | _ -> failwith ("cannot read device number from " ^ dev_file)

let device_of_name name = read_dev ("/sys/block/" ^ name ^ "/dev")

let subtract xs ys = List.filter (fun x -> not (List.mem x ys)) xs

let int_of_empty_or_string = function
  | "" -> 0
  | str -> int_of_string str

let try_sscanf str fmt f =
  try Scanf.sscanf str fmt f
  with _ -> None
