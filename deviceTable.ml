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

type sync_action' = {
  sync_speed     : int;
  sync_completed : (int64 * int64);
}

type sync_action = SyncIdle | SyncCheck of sync_action' | SyncRecover of sync_action' | SyncOther of string

type md = {
  md_dev       : device;
  devices      : device list;
  spare_devices: device list;
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

let identity a = a

let subtract xs ys = List.filter (fun x -> not (List.mem x ys)) xs

let load_md_info base =
  let md = base ^ "/md" in
  let info name format = first_line (md ^ "/" ^ name) |> format in
  let num_disks = first_line (md ^ "/raid_disks") |> int_of_string in
  let active_devices =
    CCList.range 0 (num_disks - 1)
    |> List.map (fun n -> Printf.sprintf "%s/rd%d/block/dev" md n)
    |> List.filter Sys.file_exists
    |> List.map read_dev
  in
  let all_devices =
    list_files md
    |> List.filter (pmatch ~pat:"^dev-.*")
    |> List.map (fun n ->Printf.sprintf "%s/%s/block/dev" md n)
    |> List.map read_dev
  in
  let spare_devices = subtract all_devices active_devices in
  let devices = subtract all_devices spare_devices in
  let md_dev = read_dev (base ^ "/dev") in
  let sync_action =
    let sync_action () =
      { sync_speed     = info "sync_speed" int_of_string;
        sync_completed = info "sync_completed" (fun s -> Scanf.sscanf s "%Ld / %Ld" (fun a b -> (a, b))) }
    in
    match info "sync_action" identity with
    | "recover" -> SyncRecover (sync_action ())
    | "idle" -> SyncIdle
    | "check" -> SyncCheck (sync_action ())
    | other -> SyncOther other
  in
  { md_dev; devices; spare_devices; sync_action;
    array_state    = info "array_state" (function "clean" -> `Clean | other -> `Other other);
    degraded       = info "degraded" int_of_string;
    level          = info "level" level_of_string;
    mismatch_cnt   = info "mismatch_cnt" int_of_string;
    raid_disks     = info "raid_disks" int_of_string;
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

let md_of_partitions mds partitions =
  ( mds |> CCList.filter_map @@ fun md ->
    match (partitions |> find_opt @@ fun partition ->
           List.mem partition md.devices),
          (partitions |> find_opt @@ fun partition ->
           List.mem partition md.spare_devices)with
    | None, None -> None
    | Some partition, _ -> Some (name_of_block_device md.md_dev ^ "(" ^ name_of_block_device partition ^ ")")
    | _, Some partition -> Some (name_of_block_device md.md_dev ^ "[S](" ^ name_of_block_device partition ^ ")") )
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
  let btrfs = btrfs_info () in
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
      let partitions' =
        match device with
        | None -> partitions
        | Some device -> device::partitions
      in
      let md = md_of_partitions mds partitions' in
      let mounts =
        CCList.filter_map
          (fun partition -> try Some ((List.assoc partition mounts).m_mountpoint ^ "(" ^ name_of_block_device partition ^ ")") with Not_found -> None)
          partitions'
      in
      let btrfs = btrfss_for_partitions btrfs partitions' in
      let label =
        Printf.ksprintf P.text "%s%s%s%s"
          (CCOpt.get "" device_name)
          (match md with
           | None -> ""
           | Some md -> "\n" ^ md)
          (mounts |> List.map (fun m -> "\n" ^ m) |> String.concat "")
          ((btrfs |> List.map @@ fun (partition, btrfs) -> "\n" ^ "btrfs " ^ btrfs.btrfs_label ^ "(" ^ name_of_block_device partition ^ ")") |> String.concat "")
      in
      grid.(row' + 1).(col + 1) <- label;
    done;
  done;
  P.output stdout (P.grid grid);
  Printf.printf "\n";
  let now = Unix.gettimeofday () in
  mds |> List.iter @@ fun md ->
  if md.sync_action <> SyncIdle then
    Printf.printf "%s status: %s\n%!"
      (name_of_block_device md.md_dev)
      (match md.sync_action with
       | SyncIdle  -> "idle"
       | ((SyncCheck { sync_speed; sync_completed = (at, last) })
         | (SyncRecover { sync_speed; sync_completed = (at, last) })) as action ->
         let fin = now +. Int64.(to_float last -. to_float at) /. 2.0 /. float sync_speed in
         Printf.sprintf "%s %.1f%% completed (%d kBps), ETA %s"
           (match action with
            | SyncCheck _ -> "checking"
            | SyncRecover _ -> "recovering"
            | _ -> assert false)
           Int64.(to_float at /. to_float last *. 100.0)
           sync_speed
           (string_of_time fin)
       | SyncOther other -> other)

let _ = main ()
