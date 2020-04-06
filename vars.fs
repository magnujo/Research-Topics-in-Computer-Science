module Vars

// AST of expressions with variables

type varname = string
type exp     = | INT     of int
               | ADD     of exp * exp
               | MUL     of exp * exp
               | SUP     of exp * exp
               | VAR     of varname
               | LET     of varname * exp * exp

// Interpreter

type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let rec eval env = function
  | INT i           -> i
  | ADD (e1, e2)    -> eval env e1 + eval env e2
  | SUP (e1, e2)    -> eval env e1 - eval env e2
  | MUL (e1, e2)    -> eval env e1 * eval env e2
  | VAR x           -> lookup x env
  | LET (x, e1, e2) -> let v1 = eval env e1
                       eval ((x, v1) :: env) e2

// Instructions

type inst = | IPUSH   of int
            | IGET    of int
            | IADD
            | IPOP
            | ISWAP
            | IMUL
            | ISUP

// Virtual machine

let get = List.item

let rec exec ins st =
  match (ins, st) with
    | ([],              v :: _)       -> v
    | (IPUSH i :: ins,  st)           -> exec ins (i :: st)
    | (IGET p  :: ins,  st)           -> exec ins (get p st :: st)
    | (IADD    :: ins,  y :: x :: st) -> exec ins (x + y :: st)
    | (IMUL    :: ins,  y :: x :: st) -> exec ins (x * y :: st)
    | (ISUP    :: ins,  y :: x :: st) -> exec ins (y - x :: st)
    | (IPOP    :: ins,  _ :: st)      -> exec ins st
    | (ISWAP   :: ins,  y :: x :: st) -> exec ins (x :: y :: st)

// Compiler

let rec varpos x = function
  | []       -> failwith ("unbound: " + x)
  | y :: env -> if x = y then 0 else 1 + varpos x env

let rec comp env = function
  | INT i           -> [IPUSH i]
  | ADD (e1, e2)    -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [IADD]
  | SUP (e1, e2)    -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [ISUP]
  | MUL (e1, e2)    -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [IMUL]                                          
  | VAR x           -> [IGET (varpos x env)]
  | LET (x, e1, e2) -> comp env         e1 @
                       comp (x  :: env) e2 @
                       [ISWAP]             @
                       [IPOP]
