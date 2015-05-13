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

let const a b = a

let flip f a b = f b a

let first_line file =
  contents file |> Re.replace (Re_pcre.re ~flags:[`MULTILINE] "\n.*" |> Re.compile) ~f:(const "")

type device = Device of (int * int)

type md = {
  md_dev  : device;
  devices : device list;
}

let drop_while f xs =
  let rec loop xs =
    match xs with
    | x::xs when f x -> loop xs
    | xs -> xs
  in
  loop xs

let trim_begin = Re.replace (Re_pcre.re "^ *" |> Re.compile) ~f:(const "")

let device_majors =
  let devices = CCIO.with_in "/proc/devices" CCIO.read_lines_l in
  let block_devices = devices |> drop_while (( <> ) "Block devices:") |> CCList.drop 1 |> List.map trim_begin in
  let split = block_devices |> List.map (Re.split (Re_pcre.re " " |> Re.compile)) in
  let devices = split |> CCList.filter_map (function major::name::_ -> Some (name, int_of_string major) | _ -> None) in
  devices

let partitions =
  CCIO.with_in "/proc/partitions" CCIO.read_lines_l
  |> CCList.drop 2
  |> List.map trim_begin 
  |> List.map (Re.split (Re_pcre.re "[ \t]+" |> Re.compile))
  |> CCList.filter_map @@ function
  | major::minor::_blocks::name::_ -> Some (name, Device (int_of_string major, int_of_string minor))
  | _ -> None 

let md_major = List.assoc "md" device_majors

let parent_device_of_device_name name =
  let parent = Re.replace_string (Re_pcre.re "[0-9]+$" |> Re.compile) ~by:"" name in
  if parent = name
  then None
  else Some parent

let name_of_md_device = function
  | Device (md, x) when md = md_major -> Printf.sprintf "md%d" x
  | _ -> failwith "not an md number"

let read_dev dev_file =
  first_line dev_file |> Re.split (Re_pcre.re ":" |> Re.compile) |> function
  | major::minor::_ -> Device (int_of_string major, int_of_string minor)
  | _ -> failwith ("cannot read device number from " ^ dev_file)

let disks () =
  let block_base = "/sys/block" in
  list_files block_base
  |> List.map @@ fun block ->
  let partitions =
    list_files (block_base ^ "/" ^ block)
    |> List.filter (fun d -> Sys.is_directory (block_base ^ "/" ^ block ^ "/" ^ d))
    |> List.filter (pmatch ~pat:"[0-9]$")
  in
  let partition_devices =
    partitions
    |> List.map (fun partition -> block_base ^ "/" ^ block ^ "/" ^ partition ^ "/dev")
    |> List.map read_dev
  in
  (read_dev (block_base ^ "/" ^ block ^ "/dev"),
   partition_devices)

let project1st f (x, y) = (f x, y)
let project2nd f (x, y) = (x, f y)

let project_to_2nd f x = (x, f x)

let block_devices () =
  let base = "/dev" in
  list_files base
  |> CCList.map (project_to_2nd (fun s -> base ^ "/" ^ s))
  |> CCList.map (project2nd Unix.stat)
  |> CCList.filter (fun (_, s) -> s.Unix.st_kind = Unix.S_BLK)
  |> CCList.map (project2nd @@ fun s -> Device (s.Unix.st_rdev lsr 8, s.Unix.st_rdev land 0xff))

let expand_second xs =
  xs
  |> List.map (fun (x, xs) -> xs |> List.map (fun y -> (x, y)))
  |> List.concat

let flip_list xs = List.map (fun (a, b) -> (b, a)) xs

let device_of_name name = read_dev ("/sys/block/" ^ name ^ "/dev")

let load_md_info base =
  let md = base ^ "/md" in
  let devices =
    list_files md
    |> List.filter (pmatch ~pat:"^dev-.*")
    |> List.map (fun n ->Printf.sprintf "%s/%s/block/dev" md n)
    |> List.map read_dev
  in
  let md_dev = read_dev (base ^ "/dev") in
  { md_dev; devices }

let load_all_md_info () =
  let bds = list_files_with_base "/sys/block" in
  let mds = List.filter (fun d -> Sys.file_exists (d ^ "/md")) bds in
  List.map load_md_info mds

let name_of_block_device =
  let names = flip_list @@ block_devices () in
  fun name ->
    try Some (List.assoc name names)
    with Not_found -> None

let find_opt p xs =
  try Some (List.find p xs)
  with Not_found -> None

let md_of_partitions mds partitions =
  ( mds |> CCList.filter_map @@ fun md ->
    match (partitions |> find_opt @@ fun partition ->
           List.mem partition md.devices) with
    | None -> None
    | Some partition -> Some (name_of_md_device md.md_dev ^ "(" ^ CCOpt.get "?" (name_of_block_device partition) ^ ")") )
  |> function
  | [] -> None
  | info -> Some (String.concat "\n" info)

let main () = 
  let slot_dir = "/dev/disk/by-slot" in
  let devices = List.filter (pmatch ~pat:"^[0-9]+-[0-9]+$") (list_files
  slot_dir) in
  let links =
    let full_orig = List.map (fun x -> slot_dir ^ "/" ^ x) devices in
    let links = List.map Unix.readlink full_orig in
    let shorten name = (extract ~pat:"[^/]+$" name).(0) in
    let shortlinks = List.map shorten links in
    let map = List.combine devices shortlinks in
    map
  in
  let module P = Containers_misc.PrintBox in
  let grid = Array.make_matrix 7 5 P.empty in
  let disks = disks () in
  let mds = load_all_md_info () in
  for row' = 0 to 5 do
    let row = 5 - row' in
    if row' = 0 then (
      for col = 0 to 3 do
        grid.(0).(col + 1) <- Printf.ksprintf P.text "%8d" col;
      done;
    );
    grid.(row' + 1).(0) <- Printf.ksprintf P.text "%3d" row;
    for col = 0 to 3 do
      let device_name = 
        try Some (List.assoc (Printf.sprintf "%d-%d" row col) links)
        with Not_found -> None
      in
      let device = CCOpt.map device_of_name device_name in
      let partitions = CCOpt.get [] (CCOpt.map (flip List.assoc disks) device) in
      let md = md_of_partitions mds partitions in
      let label =
        Printf.ksprintf P.text "%8s%s"
          (CCOpt.get "" device_name)
          (match md with
           | None -> ""
           | Some md -> "\n" ^ md)
      in
      grid.(row' + 1).(col + 1) <- label;
    done;
  done;
  P.output stdout (P.grid grid)

let _ = main ()
