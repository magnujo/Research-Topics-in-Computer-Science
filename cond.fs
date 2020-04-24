module Cond

// AST of a language with if expressions

type varname = string
type exp     = | INT     of int
               | ADD     of exp * exp
               | VAR     of varname
               | LET     of varname * exp * exp
               | EQ      of exp * exp
               | IF      of exp * exp * exp
               | LT      of exp * exp

// Interpreter
               
type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let rec eval env = function
  | INT i           -> i
  | ADD (e1, e2)    -> eval env e1 + eval env e2
  | VAR x           -> lookup x env
  | LET (x, e1, e2) -> let v1 = eval env e1
                       eval ((x, v1) :: env) e2
  | EQ (e1, e2)     -> if eval env e1 = eval env e2 then 1 else 0
  | LT (e1, e2)     -> if eval env e1 < eval env e2 then 1 else 0
  | IF (e1, e2, e3) -> if eval env e1 = 1 then eval env e2 else eval env e3



// Instructions

type label = int
type inst  = | IHALT
             | IPUSH   of int
             | IPOP
             | ISWAP
             | IGET    of int
             | IADD
             | IEQ
             | ILT
             | IJMP    of label
             | IJMPIF  of label
             | ILAB    of label

// Virtual machine

let get = List.item
let rec find l = function
  | (ILAB l' :: ins) -> if l = l' then ins else find l ins
  | (_       :: ins) -> find l ins

let execProg prog st =
  let rec exec ins st =
    match (ins, st) with
      | ([],                v :: _)       -> v
      | (IHALT     :: _,    v :: _)       -> v
      | (IPUSH i   :: ins,  st)           -> exec ins (i :: st)
      | (IGET p    :: ins,  st)           -> exec ins (get p st :: st)
      | (IADD      :: ins,  y :: x :: st) -> exec ins (x + y :: st)
      | (IEQ       :: ins,  y :: x :: st) ->
        if x = y then exec ins (1 :: st) else exec ins (0 :: st)
      | (ILT       :: ins,  y :: x :: st) ->
        if x < y then exec ins (1 :: st) else exec ins (0 :: st)
      | (IPOP      :: ins,  _ :: st)      -> exec ins st
      | (ISWAP     :: ins,  y :: x :: st) -> exec ins (x :: y :: st)
      | (IJMP l    :: _,    st)           -> exec (find l prog) st
      | (IJMPIF l  :: _,    1 :: st)      -> exec (find l prog) st
      | (IJMPIF l  :: ins,  _ :: st)      -> exec ins st
      | (ILAB  l   :: ins,  st)           -> exec ins st
  exec prog st

// Compiler

let rec varpos x = function
  | []       -> failwith ("unbound: " + x)
  | y :: env -> if x = y then 0 else 1 + varpos x env

let mutable labelCounter = 0
let newLabel _ =
  let this = labelCounter
  labelCounter <- this + 1;
  this

let rec comp env = function
  | INT i           -> [IPUSH i]
  | ADD (e1, e2)    -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [IADD]
  | EQ (e1, e2)     -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [IEQ]
  | LT (e1, e2)     -> comp env         e1 @
                       comp ("" :: env) e2 @
                       [ILT]                    
  | VAR x           -> [IGET (varpos x env)]
  | LET (x, e1, e2) -> comp env        e1 @
                       comp (x :: env) e2 @
                       [ISWAP]            @
                       [IPOP]
  | IF (e1, e2, e3) -> let l2 = newLabel()
                       let le = newLabel()
                       comp env e1  @
                       [IJMPIF l2]  @
                       comp  env e3 @
                       [IJMP le]    @
                       [ILAB l2]    @
                       comp env e2  @
                       [ILAB le]

comp ["x"; "y" "z"] (ADD(INT 1, VAR "x"))