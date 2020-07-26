//Lister i F#

//Creating lists
let l = [1; 2; 3]
let l' = 1::2::3::[]
let tail = [4; 5]
let lt = 1::2::3::tail

//list functions
let rec len = function 
  | []      -> 0 
  | x :: xs -> 1 + len xs

let rec sum = function
  | []      -> 0 
  | x :: xs -> x + sum xs

let rec append xs ys =
  match xs with 
    | []      -> ys
    | x :: xs -> x :: append xs ys

//Common built-in functions on lists 
let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs
let get = List.item
