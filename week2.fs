//1

let rec product = function
  |[] -> 0 
  |x::xs -> x + product(xs)

//2
let rec count (x, ys) = 
  match ys with
    |[]      -> 0 
    |y :: ys -> if x = y then 1 + count(x, ys) else count(x, ys)

count(4, [])

//3

let rec sum(p, xs) = 
  match xs with
    |[]         -> 0 
    |x :: xs    -> if p x = true then 1 + sum(p, xs) else sum(p, xs)


let p x = x>0

sum(p, [2;1;3;1;1])
                      

