// Functional programming and F#

//int types
let i = 10 
let exp = 10 + 11

//string type
let h = "hello"

//function
let eq x y = if x = y then 1 else 0

//tuples
let det ((a, c), (b, d)) = a * d - b * c

//pattern matching
let p = function
    | 0 -> "false"
    | 1 -> "true"

p (eq 11 10) 

//recursive function
let rec sumupTo = function
    |0 -> 0
    |n -> n + sumupTo(n - 1)


//types (sets of values)
type weekday = | MONDAY    | TUESDAY   | WEDNESDAY   | THURSDAY
               | FRIDAY    | SATURDAY  | SUNDAY

