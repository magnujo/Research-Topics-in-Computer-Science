module Week2

// Lists

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

let rec upto (m, n) =
  if m > n then
    []
  else
    m :: upto (m + 1, n)

// Quicksort

let rec partition p = function
  | []      -> ([], [])
  | y :: ys -> let (l, r) = partition p ys
               if y <= p then
                 (y :: l, r)
               else
                 (l, y :: r)

               

let rec sort = function
  | []        -> []
  | [x]       -> [x]
  | x :: xs   -> let (l,r) = partition x xs
                 sort l @ [x] @ sort r

sort [2;1;10;5;4;33]


(* 
sort 2::1::[] --> (l,r) = partition 2 1::[] = (1::[], [])
                         sort l @ 2 @ sort r = sort 1::[] @ 2 @ sort [] = 1::[] @ 2 @ [] = 1::2::[]
partition 2 1::[] --> (l, r) = partition 2 [] =([], [])
                      (1::l, r) = (1::[], [])
partition 2 [] ---> ([], [])

sort 1::[] --> 1::[]
sort [] --> []
 *)

// merge sort

let rec split = function
  | []      -> ([], [])
  | [x1]    -> ([x1], [])
  | x1 :: x2 :: xs -> let (l, r) = split xs
                      (x1 :: l, x2 :: r)

// Ex 5(a)
let rec isWeaklyAscending = function
  | []             -> true
  | [x1]           -> true
  | x1 :: x2 :: xs -> if x1 <= x2 then
                        isWeaklyAscending (x2 :: xs)
                      else
                        false


let rec intersect = function
  |(ys, []) -> []  
  |([], xs) -> []  
  |(x::xs, y::ys) -> if x = y then x::intersect(xs, y::ys)
                     else if x < y then intersect(xs, y::ys)  
                     else intersect (x::xs, ys)  


let rec union xs ys =
  match (xs, ys) with
    | (x :: xs, y :: ys) -> if x < y then
                              x :: union xs (y :: ys)
                            else
                              y :: union (x :: xs) ys
    | ([], ys) -> ys
    | (xs, []) -> xs

// Ex 6

let rec mergesort = function
  | []   -> []
  | [x]  -> [x]
  | xs   -> let (l, r) = split xs
            union (mergesort l) (mergesort r)


//search
let rec maxSearch max =  function
        | []      -> max
        | x :: xs -> if x > max then maxSearch x xs else maxSearch max xs
                      
let max = 0
let rec maxSearch ys =
    match ys with
       | []      -> max
       | x :: xs -> if x > max then maxSearch xs else maxSearch xs
                    

