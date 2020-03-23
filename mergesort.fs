module Mergesort

// Ex 4
// split [x1; x2; x3; x4; ...] = ([x1; x3; ...], [x2; x4; ...])

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

// Ex 5(d)

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
