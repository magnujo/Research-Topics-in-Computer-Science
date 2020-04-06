module Week3

// Trees

type 'a tree = 
  | LEAF
  | NODE of 'a * 'a tree * 'a tree

let t = NODE (9, NODE (3, LEAF, 
                          NODE (7, LEAF, LEAF)), 
                 NODE (1, LEAF, LEAF))

// Depth of a tree

let rec depth = function 
  | LEAF           -> 0 
  | NODE (x, l, r) -> 1 + max (depth l) (depth r)

// In-order traversal
    
let rec inorder = function 
  | LEAF           -> []
  | NODE (x, l, r) -> inorder l @ [x] @ inorder r

// Add element to ordered tree

let rec add v = function
  | LEAF           -> NODE (v, LEAF, LEAF)
  | NODE (w, l, r) -> if v <= w then
                        NODE (w, add v l, r)
                      else 
                        NODE (w, l, add v r)

// Make an ordered tree from a list

let rec makeTree = function
  | []      -> LEAF
  | x :: xs -> add x (makeTree xs)

let treesort xs = inorder (makeTree xs)

// AST of arithmetic expressions

type exp = | INT of int
           | ADD of exp * exp

// Interpreter for arithmetic expressions

let rec eval = function
  | INT i        -> i
  | ADD (e1, e2) -> eval e1 + eval e2           
