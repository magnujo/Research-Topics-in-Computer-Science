//1a

//Træer med 3 "grene"
type 'a tree = | LEAF
               | NODE of 'a * 'a tree * 'a tree 


let t = NODE(5, NODE(3, LEAF, NODE(2, LEAF, LEAF)), NODE(1, NODE(4, LEAF, LEAF), LEAF))

(* 
                                                    NODE(5, 
                                    NODE(3,                         NODE(1, 
                              LEAF,     NODE(2,               NODE(4       LEAF
                                      LEAF, LEAF),         LEAF, LEAF                  *)
let p = LEAF


//Tree functions
let rec countLeaves = function
  | LEAF -> 0
  | NODE (x, LEAF, LEAF) -> 2
  | NODE (x, LEAF, r) -> 1 + countLeaves r 
  | NODE (x, l, LEAF) -> 1 + countLeaves l 
  | NODE (x, l, r) -> (countLeaves l) + (countLeaves r) 

let rec countNodes = function
    | LEAF -> 0 
    | NODE (x, l, r) -> 1 + (countNodes l) + (countNodes r) 


countNodes t


let rec sum = function 
    |LEAF -> 0 
    |NODE (x, l, r ) -> x + sum l + sum r
 
sum t


let rec product = function 
    |LEAF -> 1 
    |NODE (x, l, r) -> x*sum l * sum r 

product t


let rec double = function 
    |LEAF -> LEAF
    |NODE (x, l, r) -> NODE (x*2, double l, double r)
 


let rec depth = function 
  | LEAF           -> 0 
  | NODE (x, l, r) -> 1 + max (depth l) (depth r)

// In-order traversal
    
let rec inorder = function 
  | LEAF           -> []
  | NODE (x, l, r) -> inorder l @ [x] @ inorder r

inorder t


//tilføjer nodes til træ i en ordered rækkefølge
let rec add v = function
  | LEAF           -> NODE (v, LEAF, LEAF)  
  | NODE (w, l, r) -> if v <= w then
                        NODE (w, add v l, r)  
                      else 
                        NODE (w, l, add v r) 

//lav et ordered træ ud af en liste
let rec makeTree = function
  | []      -> LEAF
  | x :: xs -> add x (makeTree xs)  


//"printer" træet 
let treesort xs = inorder (makeTree xs)


treesort [3; 2; 5]

// AST of arithmetic expressions
type exp = | INT of int
           | ADD of exp * exp

// Interpreter for arithmetic expressions

let rec eval = function
  | INT i        -> i
  | ADD (e1, e2) -> eval e1 + eval e2           
