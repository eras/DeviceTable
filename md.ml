open Util.Op

type md_device_state' =
  | DS_Faulty
  | DS_InSync
  | DS_WriteMostly
  | DS_Blocked
  | DS_Spare
  | DS_Write
  | DS_WantReplacement
  | DS_Replacement
  | DS_Unsupported of string

type md_device_state = md_device_state' list
      
let md_device_state_of_string state =
  Re.split (Re_pcre.re "," |> Re.compile) state |> List.map @@ function
    | "faulty"           -> DS_Faulty
    | "in_sync"          -> DS_InSync
    | "writemostly"      -> DS_WriteMostly
    | "blocked"          -> DS_Blocked
    | "spare"            -> DS_Spare
    | "write"            -> DS_Write
    | "want_replacement" -> DS_WantReplacement
    | "replacement"      -> DS_Replacement
    | other              -> DS_Unsupported other

type md_device_info = {
  md_di_device : Types.device;
  md_di_state : md_device_state;
}

type sync_action' = {
  sync_speed     : int;
  sync_completed : (int64 * int64);
}

type reshape = {
  reshape_sync_action : sync_action';
  reshape_to_disks    : int;
  reshape_from_disks  : int option;
}

type sync_action =
  | SyncIdle
  | SyncCheck of sync_action'
  | SyncRecover of sync_action'
  | SyncReshape of reshape
  | SyncOther of string
  | SyncStopped

type t = {
  md_dev          : Types.device;
  devices         : md_device_info list;
  array_state     : [`Clean | `Other of string];
  degraded        : int;
  level           : [`Raid0 | `Raid1 | `Raid5 | `Raid6 | `Other of string];
  mismatch_cnt    : int;
  raid_disks      : int;
  prev_raid_disks : int option;
  sync_action     : sync_action;
}

let md_info_of_partitions mds partitions =
  let character_of_device_state = function
    | DS_Faulty -> "F"
    | DS_InSync -> ""
    | DS_WriteMostly -> "W"
    | DS_Blocked -> "B"
    | DS_Spare -> "S"
    | DS_Write -> "W"
    | DS_WantReplacement -> "R"
    | DS_Replacement -> "r"
    | DS_Unsupported str -> "Unsupported(" ^ str ^ ")"
  in
  ( mds |> CCList.filter_map @@ fun md ->
    match (partitions |> CCList.filter_map @@ fun partition ->
           Util.find_opt (fun di -> di.md_di_device = partition) md.devices)
    with
    | [] -> None
    | { md_di_state; md_di_device }::insert_code ->
       let ch = String.concat "" @@ List.map character_of_device_state md_di_state in
       let ch = if ch = "" then "" else "[" ^ ch ^ "]" in
       Some (Util.name_of_block_device md.md_dev ^ ch ^ "(" ^ Util.name_of_block_device md_di_device ^ ")") )
  |> function
  | [] -> None
  | info -> Some (String.concat "\n" info)
