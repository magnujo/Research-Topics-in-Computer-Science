module Arith

// AST of arithmetic expressions
type exp = | INT of int
           | ADD of exp * exp

// Interpreter
let rec eval = function
  | INT i        -> i
  | ADD (e1, e2) -> eval e1 + eval e2

// Instructions
type inst = | IPUSH of int
            | IADD

// Virtual machine
let rec exec ins st =
  match (ins, st) with
    | ([],             v :: _)       -> v
    | (IPUSH i :: ins, st)           -> exec ins (i     :: st)
    | (IADD    :: ins, y :: x :: st) -> exec ins (x + y :: st)

// Compiler
let rec comp = function
  | INT i        -> [IPUSH i]
  | ADD (e1, e2) -> let ins1 = comp e1
                    let ins2 = comp e2
                    ins1 @ ins2 @ [IADD]
