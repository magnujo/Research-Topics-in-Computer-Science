//1a


(* type 'a tree = | LEAF
               | NODE of 'a * 'a tree * 'a tree *)


let t = NODE(5, NODE(3, LEAF, NODE(2, LEAF, LEAF)), NODE(1, NODE(4, LEAF, LEAF), LEAF))
let p = LEAF
//1b

let rec countLeaves = function
  | LEAF -> 0
  | NODE (x, LEAF, LEAF) -> 2
  | NODE (x, LEAF, r) -> 1 + countLeaves r 
  | NODE (x, l, LEAF) -> 1 + countLeaves l 
  | NODE (x, l, r) -> (countLeaves l) + (countLeaves r) 


//1c 
let rec countNodes = function
    | LEAF -> 0 
    | NODE (x, l, r) -> 1 + (countNodes l) + (countNodes r) 


countNodes t

//1d

let rec sum = function 
    |LEAF -> 0 
    |NODE (x, l, r ) -> x + sum l + sum r
 
sum t

//1e 

let rec product = function 
    |LEAF -> 1 
    |NODE (x, l, r) -> x*sum l * sum r 

product t


//1f
let rec double = function 
    |LEAF -> LEAF
    |NODE (x, l, r) -> NODE (x*2, double l, double r)
 
//2a

type 'a tree = | LEAF of 'a
               | NODE of 'a tree * 'a tree

let t = NODE (
             NODE(LEAF 1, LEAF 2), 
             NODE(LEAF 4, LEAF 0))
//2b
let rec sum = function 
 |NODE (LEAF l, LEAF r) -> l + r
 |NODE (l, LEAF r) -> sum l + r
 |NODE (LEAF l, r) -> l + sum r
 |NODE (l, r) -> sum l + sum r

sum t 

//2c
let rec leaves = function
 |NODE (LEAF l, LEAF r) -> l::r::[]
 |NODE (LEAF l, r) -> l::leaves r
 |NODE (l, LEAF r) -> r::leaves l
 |NODE (l, r)      -> (leaves l) @ (leaves r)

leaves t