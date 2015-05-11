let list_files dir =
  let dir = Unix.opendir dir in
  let rec get_files aux =
    match try Some (Unix.readdir dir)
          with End_of_file -> None 
          with
    | Some file -> get_files (file::aux)
    | None -> List.rev aux
  in
  let files = get_files [] in
  Unix.closedir dir;
  files

let main () = 
  let slot_dir = "/dev/disk/by-slot" in
  let devices = List.filter (Pcre.pmatch ~pat:"^[0-9]+-[0-9]+$") (list_files
  slot_dir) in
  let links =
    let full_orig = List.map (fun x -> slot_dir ^ "/" ^ x) devices in
    let links = List.map Unix.readlink full_orig in
    let shorten name = (Pcre.extract ~pat:"[^/]+$" name).(0) in
    let shortlinks = List.map shorten links in
    let map = List.combine devices shortlinks in
    map
  in
  for row' = 0 to 5 do
    let row = 5 - row' in
    if row' = 0 then (
      Printf.printf "     ";
      for col = 0 to 3 do
        Printf.printf "%8d" col;
      done;
      Printf.printf "\n";
    );
    Printf.printf "%3d " row;
    for col = 0 to 3 do
      let disk = 
        try Some (List.assoc (Printf.sprintf "%d-%d" row col) links)
        with Not_found -> None
      in
      Printf.printf "%8s" (match disk with None -> "" | Some disk -> disk)
    done;
    Printf.printf "\n"
  done

let _ = main ()
