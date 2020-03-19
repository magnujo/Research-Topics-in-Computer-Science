module Week2

// Tuples

let today  = (2020, 3, 16)
let cicero = ("Cicero", (-106, 1, 3))  // A Roman, born 106 BC

let year (y, m, d)     = y             // Pattern match
let birthday (name, d) = d             // Pattern match
let age p = 2020 - year (birthday p)

let age_of_cicero = age ("Cicero", (-106, 1, 3))   // 2126 (!)

let nextYear (y, m, d) = (y + 1, m, d)

// Tagged values
  
type weekday = | MONDAY    | TUESDAY   | WEDNESDAY   | THURSDAY
               | FRIDAY    | SATURDAY  | SUNDAY

let isWeekend = function
                  | SATURDAY -> true
                  | SUNDAY   -> true
                  | _        -> false

type intOption = | NONE 
                 | SOME of int

let workload = function 
                 | SATURDAY -> NONE
                 | SUNDAY   -> NONE
                 | FRIDAY   -> SOME 6
                 | _        -> SOME 8

let workloads = List.map workload [MONDAY; TUESDAY; WEDNESDAY; THURSDAY; FRIDAY; SATURDAY; SUNDAY]

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
                 (l,      y :: r)

let rec sort = function
  | []        -> []
  | [x]       -> [x]
  | p :: xs   -> let (l,r) = partition p xs
                 sort l @ [p] @ sort r
