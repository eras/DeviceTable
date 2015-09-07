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

let (^/) a b = a ^ "/" ^ b      
