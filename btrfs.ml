type t = {
  btrfs_id      : string;
  btrfs_label   : string;
  btrfs_devices : Types.device list;
}

let btrfs_info () =
  let base = "/sys/fs/btrfs" in
  let fs_ids = Util.list_files base |> List.filter (Util.pmatch ~pat:"^.+-.+-.+-.+-.+$") in
  fs_ids |> List.map @@ fun fs_id ->
  let fs_base = base ^ "/" ^ fs_id in
  let devices = Util.list_files_with_base (fs_base ^ "/devices") |> List.map (fun s -> s ^ "/dev") |> List.map Util.read_dev in
  { btrfs_id = fs_id;
    btrfs_label = Util.first_line (fs_base ^ "/label");
    btrfs_devices = devices; }

let btrfss_for_partitions btrfs partitions =
  let btrfs_for_partition partition =
    btrfs |> CCList.filter_map @@ fun btrfs ->
    if List.mem partition btrfs.btrfs_devices
    then Some (partition, btrfs)
    else None
  in
  CCList.map btrfs_for_partition partitions |> List.concat
