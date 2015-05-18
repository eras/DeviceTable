let identity a = a

let const a b = a

let flip f a b = f b a

let sum = List.fold_left ( + ) 0

let project1st f (x, y) = (f x, y)

let project2nd f (x, y) = (x, f y)

let project_to_2nd f x = (x, f x)

