open Util

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

type device = Device of (int * int)

type device_state' =
  | DS_Faulty
  | DS_InSync
  | DS_WriteMostly
  | DS_Blocked
  | DS_Spare
  | DS_Write
  | DS_WantReplacement
  | DS_Unsupported of string

type device_state = device_state' list
      
let device_state_of_string state =
  Re.split (Re_pcre.re "," |> Re.compile) state |> List.map @@ function
    | "faulty"           -> DS_Faulty
    | "in_sync"          -> DS_InSync
    | "writemostly"      -> DS_WriteMostly
    | "blocked"          -> DS_Blocked
    | "spare"            -> DS_Spare
    | "write"            -> DS_Write
    | "want_replacement" -> DS_WantReplacement
    | other              -> DS_Unsupported other

type device_info = {
  di_device : device;
  di_state : device_state;
}

type sync_action' = {
  sync_speed     : int;
  sync_completed : (int64 * int64);
}

type sync_action =
  | SyncIdle
  | SyncCheck of sync_action'
  | SyncRecover of sync_action'
  | SyncOther of string
  | SyncStopped

type md = {
  md_dev       : device;
  devices      : device_info list;
  array_state  : [`Clean | `Other of string];
  degraded     : int;
  level        : [`Raid0 | `Raid1 | `Raid5 | `Raid6 | `Other of string];
  mismatch_cnt : int;
  raid_disks   : int;
  sync_action  : sync_action;
}

let drop_while f xs =
  let rec loop xs =
    match xs with
    | x::xs when f x -> loop xs
    | xs -> xs
  in
  loop xs

let trim_begin = Re.replace (Re_pcre.re "^ *" |> Re.compile) ~f:(const "")

let dev_filesystems =
  CCIO.with_in "/proc/filesystems" CCIO.read_lines_l
  (* TODO: really the prefixing whitespace should be part of the split as well *)
  |> List.map (Re.split (Re_pcre.re "\t" |> Re.compile))
  |> CCList.filter_map @@ function
  | "nodev"::_ -> None
  | filesystem::[] -> Some filesystem
  | other ->
    Printf.eprintf "device-table: warning: unknown entry %s in /proc/filesystem\n%!"
      (String.concat "," other);
    None

let device_of_stat s = Device (s.Unix.st_rdev lsr 8, s.Unix.st_rdev land 0xff)

type mount = {
  m_mountpoint : string;
  m_filesystem : string;
}

let mounts =
  CCIO.with_in "/proc/mounts" CCIO.read_lines_l
  |> List.map (Re.split (Re_pcre.re " " |> Re.compile))
  |> CCList.filter_map @@ function
  | "rootfs"::_ ->
    None
  | device::m_mountpoint::m_filesystem::flags::_nr1::_nr2::_ when List.mem m_filesystem dev_filesystems ->
    ( try Some (device_of_stat (Unix.stat device), { m_mountpoint; m_filesystem })
      with exn ->
        Printf.eprintf "Error while statting %s: %s\n" device (Printexc.to_string exn);
        None )
  | _ -> None

let partitions =
  CCIO.with_in "/proc/partitions" CCIO.read_lines_l
  |> CCList.drop 2
  |> List.map trim_begin 
  |> List.map (Re.split (Re_pcre.re "[ \t]+" |> Re.compile))
  |> CCList.filter_map @@ function
  | major::minor::_blocks::name::_ -> Some (name, Device (int_of_string major, int_of_string minor))
  | _ -> None 

let parent_device_of_device_name name =
  let parent = Re.replace_string (Re_pcre.re "[0-9]+$" |> Re.compile) ~by:"" name in
  if parent = name
  then None
  else Some parent

let read_dev dev_file =
  first_line dev_file |> Re.split (Re_pcre.re ":" |> Re.compile) |> function
  | major::minor::_ -> Device (int_of_string major, int_of_string minor)
  | _ -> failwith ("cannot read device number from " ^ dev_file)

let device_info base =
  { di_device = read_dev (base ^/ "block/dev");
    di_state = device_state_of_string (first_line (base ^/ "state")) }

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

let block_devices () =
  let base = "/dev" in
  list_files base
  |> CCList.map (project_to_2nd (fun s -> base ^ "/" ^ s))
  |> CCList.map (project2nd Unix.stat)
  |> CCList.filter (fun (_, s) -> s.Unix.st_kind = Unix.S_BLK)
  |> CCList.map (project2nd @@ device_of_stat)

let expand_second xs =
  xs
  |> List.map (fun (x, xs) -> xs |> List.map (fun y -> (x, y)))
  |> List.concat

let flip_list xs = List.map (fun (a, b) -> (b, a)) xs

let device_of_name name = read_dev ("/sys/block/" ^ name ^ "/dev")

let level_of_string = function
  | "raid0" -> `Raid0
  | "raid1" -> `Raid1
  | "raid5" -> `Raid5
  | "raid6" -> `Raid6
  | other -> `Other other

let subtract xs ys = List.filter (fun x -> not (List.mem x ys)) xs

let int_of_empty_or_string = function
  | "" -> 0
  | str -> int_of_string str

let load_md_info base =
  let md = base ^ "/md" in
  let info name format = first_line (md ^ "/" ^ name) |> format in
  let info_opt name format =
    try Some (first_line (md ^ "/" ^ name) |> format)
    with Sys_error _ -> None
  in
  (* let num_disks = first_line (md ^ "/raid_disks") |> int_of_empty_or_string in *)
  let devices =
    list_files md
    |> List.filter (pmatch ~pat:"^dev-.*")
    |> List.map (fun n -> Printf.sprintf "%s/%s" md n)
    |> List.map device_info
  in
  let md_dev = read_dev (base ^ "/dev") in
  let sync_action =
    let sync_action () =
      { sync_speed     = info "sync_speed" int_of_string;
        sync_completed = info "sync_completed" (fun s -> Scanf.sscanf s "%Ld / %Ld" (fun a b -> (a, b))) }
    in
    match info_opt "sync_action" identity with
    | Some "recover" -> SyncRecover (sync_action ())
    | Some "idle" -> SyncIdle
    | Some "check" -> SyncCheck (sync_action ())
    | Some other -> SyncOther other
    | None -> SyncStopped
  in
  { md_dev; devices; sync_action;
    array_state    = info "array_state" (function "clean" -> `Clean | other -> `Other other);
    degraded       = CCOpt.get 0 (info_opt "degraded" int_of_string);
    level          = info "level" level_of_string;
    mismatch_cnt   = CCOpt.get 0 (info_opt "mismatch_cnt" int_of_string);
    raid_disks     = info "raid_disks" int_of_empty_or_string;
  }

let load_all_md_info () =
  let bds = list_files_with_base "/sys/block" in
  let mds = List.filter (fun d -> Sys.file_exists (d ^ "/md")) bds in
  List.map load_md_info mds

let name_of_block_device =
  let names = flip_list @@ block_devices () in
  fun ((Device (major, minor)) as device) ->
    try List.assoc device names
    with Not_found -> Printf.sprintf "%d:%d" major minor

let find_opt p xs =
  try Some (List.find p xs)
  with Not_found -> None

type btrfs = {
  btrfs_id      : string;
  btrfs_label   : string;
  btrfs_devices : device list;
}

let btrfs_info () =
  let base = "/sys/fs/btrfs" in
  let fs_ids = list_files base |> List.filter (pmatch ~pat:"^.+-.+-.+-.+-.+$") in
  fs_ids |> List.map @@ fun fs_id ->
  let fs_base = base ^ "/" ^ fs_id in
  let devices = list_files_with_base (fs_base ^ "/devices") |> List.map (fun s -> s ^ "/dev") |> List.map read_dev in
  { btrfs_id = fs_id;
    btrfs_label = first_line (fs_base ^ "/label");
    btrfs_devices = devices; }

let md_info_of_partitions mds partitions =
  let character_of_device_state = function
    | DS_Faulty -> "F"
    | DS_InSync -> ""
    | DS_WriteMostly -> "W"
    | DS_Blocked -> "B"
    | DS_Spare -> "S"
    | DS_Write -> "W"
    | DS_WantReplacement -> "R"
    | DS_Unsupported str -> "Unsupported(" ^ str ^ ")"
  in
  ( mds |> CCList.filter_map @@ fun md ->
    match (partitions |> CCList.filter_map @@ fun partition ->
           find_opt (fun di -> di.di_device = partition) md.devices)
    with
    | [] -> None
    | { di_state; di_device }::insert_code ->
       let ch = String.concat "" @@ List.map character_of_device_state di_state in
       let ch = if ch = "" then "" else "[" ^ ch ^ "]" in
       Some (name_of_block_device md.md_dev ^ ch ^ "(" ^ name_of_block_device di_device ^ ")") )
  |> function
  | [] -> None
  | info -> Some (String.concat "\n" info)

let btrfss_for_partitions btrfs partitions =
  let btrfs_for_partition partition =
    btrfs |> CCList.filter_map @@ fun btrfs ->
    if List.mem partition btrfs.btrfs_devices
    then Some (partition, btrfs)
    else None
  in
  CCList.map btrfs_for_partition partitions |> List.concat

let string_of_tm { Unix.tm_sec = sec;
                   tm_min = min;
                   tm_hour = hour;
                   tm_mday = mday;
                   tm_mon = mon;
                   tm_year = year } =
  Printf.sprintf
    "%04d-%02d-%02d %02d:%02d:%02d"
    (year + 1900)
    (mon + 1)
    (mday)
    (hour)
    (min)
    (sec)

let string_of_time t =
  string_of_tm (Unix.localtime t)

type layout = {
  l_devices_per_row : int list;
}

let layout = {
  l_devices_per_row = [4; 4; 4; 4; 4; 4];
}

type ctx = {
  links : (string * string) list;
  disks : (device * device list) list;
  mds : md CCList.t;
  btrfs : btrfs list;
}

let label_of_position ctx row col =
  let module P = Containers_misc.PrintBox in
  let device_name = 
    try Some (List.assoc (Printf.sprintf "%d-%d" row col) ctx.links)
    with Not_found -> None
  in
  let device = CCOpt.map device_of_name device_name in
  let partitions = CCOpt.get [] (CCOpt.map (flip List.assoc ctx.disks) device) in
  let partitions' =
    match device with
    | None -> partitions
    | Some device -> device::partitions
  in
  let md = md_info_of_partitions ctx.mds partitions' in
  let mounts =
    CCList.filter_map
      (fun partition -> try Some ((List.assoc partition mounts).m_mountpoint ^ "(" ^ name_of_block_device partition ^ ")") with Not_found -> None)
      partitions'
  in
  let btrfs = btrfss_for_partitions ctx.btrfs partitions' in
  let label =
    Printf.ksprintf P.text "%s%s%s%s"
      (CCOpt.get "" device_name)
      (match md with
       | None -> ""
       | Some md -> "\n" ^ md)
      (mounts |> List.map (fun m -> "\n" ^ m) |> String.concat "")
      ((btrfs |> List.map @@ fun (partition, btrfs) -> "\n" ^ "btrfs " ^ btrfs.btrfs_label ^ "(" ^ name_of_block_device partition ^ ")") |> String.concat "")
  in
  label

let main () =
  (* Containers_misc.PrintBox.set_string_len Text.length; *)
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
  let ctx =
    let disks = disks () in
    let mds   = load_all_md_info () in
    let btrfs = btrfs_info () in
    { links; disks; mds; btrfs }
  in
  for col = 0 to 3 do
    grid.(0).(col + 1) <- Printf.ksprintf P.text "%8d" col;
  done;
  ( layout.l_devices_per_row
    |> flip List.fold_left 0 @@ fun row' num_devices ->
    let row = 5 - row' in
    grid.(row' + 1).(0) <- Printf.ksprintf P.text "%3d" row;
    for col = 0 to 3 do
      let label = label_of_position ctx row col in
      grid.(row' + 1).(col + 1) <- label;
    done;
    row' + 1 ) |> ignore;
  (* let layout = P.grid grid in *)
  let layout = List.map Array.to_list (Array.to_list grid) |> Layout.horizontal_lists in
  P.output stdout layout;
  Printf.printf "\n";
  let now = Unix.gettimeofday () in
  ctx.mds |> List.iter @@ fun md ->
  if md.sync_action <> SyncIdle then
    Printf.printf "%s status: %s\n%!"
      (name_of_block_device md.md_dev)
      (match md.sync_action with
       | SyncIdle  -> "idle"
       | (SyncCheck sync_action | SyncRecover sync_action) as action ->
         let { sync_speed; sync_completed = (at, last) } = sync_action in
         let fin = now +. Int64.(to_float last -. to_float at) /. 2.0 /. float sync_speed in
         Printf.sprintf "%s %.1f%% completed (%.1f Mi/s), ETA %s"
           (match action with
            | SyncCheck _ -> "checking"
            | SyncRecover _ -> "recovering"
            | _ -> assert false)
           Int64.(to_float at /. to_float last *. 100.0)
           (float sync_speed /. 1024.)
           (string_of_time fin)
       | SyncOther other -> other
       | SyncStopped -> "stopped")

let _ = main ()
