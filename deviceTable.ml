open Util

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
  | major::minor::_blocks::name::_ -> Some (name, Types.Device (int_of_string major, int_of_string minor))
  | _ -> None 

let parent_device_of_device_name name =
  let parent = Re.replace_string (Re_pcre.re "[0-9]+$" |> Re.compile) ~by:"" name in
  if parent = name
  then None
  else Some parent

let md_device_info base =
  { Md.md_di_device = Util.read_dev (base ^/ "block/dev");
    md_di_state = Md.md_device_state_of_string (first_line (base ^/ "state")) }

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
    |> List.map Util.read_dev
  in
  (Util.read_dev (block_base ^ "/" ^ block ^ "/dev"),
   partition_devices)

let level_of_string = function
  | "raid0" -> `Raid0
  | "raid1" -> `Raid1
  | "raid5" -> `Raid5
  | "raid6" -> `Raid6
  | other -> `Other other

let raid_disks_of_string str =
  match try_sscanf str "%d (%d)" (fun x y -> Some (x, y)) with
  | Some (x, y) -> (x, Some y)
  | None ->
    match try_sscanf str "%d" (fun x -> Some x) with
    | Some x -> (x, None)
    | None -> (0, None)

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
    |> List.map md_device_info
  in
  let md_dev = Util.read_dev (base ^ "/dev") in
  let raid_disks, prev_raid_disks = info "raid_disks" raid_disks_of_string in
  let sync_action =
    let open Md in
    let sync_action () =
      { sync_speed  = info "sync_speed" int_of_string;
        sync_completed = info "sync_completed" (fun s -> Scanf.sscanf s "%Ld / %Ld" (fun a b -> (a, b))) }
    in
    match info_opt "sync_action" identity with
    | Some "recover" -> SyncRecover (sync_action ())
    | Some "idle" -> SyncIdle

    | Some "reshape" -> SyncReshape { reshape_sync_action = sync_action ();
                                      reshape_to_disks = raid_disks;
                                      reshape_from_disks = prev_raid_disks }
    | Some other -> SyncOther other
    | None -> SyncStopped
  in
  { Md.md_dev; devices; sync_action;
    array_state    = info "array_state" (function "clean" -> `Clean | other -> `Other other);
    degraded       = CCOpt.get 0 (info_opt "degraded" int_of_string);
    level          = info "level" level_of_string;
    mismatch_cnt   = CCOpt.get 0 (info_opt "mismatch_cnt" int_of_string);
    raid_disks;
    prev_raid_disks;
  }

let load_all_md_info () =
  let bds = list_files_with_base "/sys/block" in
  let mds = List.filter (fun d -> Sys.file_exists (d ^ "/md")) bds in
  List.map load_md_info mds

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
  disks : (Types.device * Types.device list) list;
  mds : Md.t CCList.t;
  btrfs : Btrfs.t list;
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
  let md = Md.md_info_of_partitions ctx.mds partitions' in
  let mounts =
    CCList.filter_map
      (fun partition -> try Some ((List.assoc partition mounts).m_mountpoint ^ "(" ^ name_of_block_device partition ^ ")") with Not_found -> None)
      partitions'
  in
  let btrfs = Btrfs.btrfss_for_partitions ctx.btrfs partitions' in
  let label =
    Printf.ksprintf P.text "%s%s%s%s"
      (CCOpt.get "" device_name)
      (match md with
       | None -> ""
       | Some md -> "\n" ^ md)
      (mounts |> List.map (fun m -> "\n" ^ m) |> String.concat "")
      ((btrfs |> List.map @@ fun (partition, btrfs) -> "\n" ^ "btrfs " ^ btrfs.Btrfs.btrfs_label ^ "(" ^ name_of_block_device partition ^ ")") |> String.concat "")
  in
  label

let string_of_sync_action' sync_action description =
  let now = Unix.gettimeofday () in
  let { Md.sync_speed; sync_completed = (at, last) } = sync_action in
  let fin = now +. Int64.(to_float last -. to_float at) /. 2.0 /. float sync_speed in
  Printf.sprintf "%s %.1f%% completed (%.1f Mi/s), ETA %s"
    description
    Int64.(to_float at /. to_float last *. 100.0)
    (float sync_speed /. 1024.)
    (string_of_time fin)

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
    let btrfs = Btrfs.btrfs_info () in
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
  ctx.mds |> List.iter @@ fun md ->
  if md.Md.sync_action <> SyncIdle then
    Printf.printf "%s status: %s\n%!"
      (name_of_block_device md.md_dev)
      (match md.sync_action with
       | SyncIdle  -> "idle"
       | (SyncCheck sync_action | SyncRecover sync_action) as action ->
         let description = match action with
           | SyncCheck _ -> "checking"
           | SyncRecover _ -> "recovering"
           | _ -> assert false
         in
         string_of_sync_action' sync_action description
       | SyncReshape reshape ->
           string_of_sync_action' reshape.reshape_sync_action (
             "reshaping " ^
             ( match reshape.reshape_from_disks with
               | None -> "unknown"
               | Some n -> string_of_int n )
             ^ " => " ^ string_of_int reshape.reshape_to_disks
             ^ " disks"
           )
       | SyncOther other -> other
       | SyncStopped -> "stopped")

let _ = main ()
