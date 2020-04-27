module FoFunc

// AST of a language with functions

type varname  = string
type funcname = string
type exp      = | INT     of int             // i
                | ADD     of exp * exp       // e1 + e2
                | VAR     of varname         // x
                | CALL    of funcname * exp  // f ( e )                
type func     = funcname * (varname * exp)   // func f ( x ) = e

// Interpreter

type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) =
  let rec eval env = function
    | INT i           -> i
    | ADD (e1, e2)    -> eval env e1 + eval env e2
    | VAR x           -> lookup x env
    | CALL (f, e)     -> let v = eval env e
                         let (x, body) = lookup f funcs
                         eval [(x, v)] body
  eval [] e

// Instructions

type label = int
type inst  = | IHALT
             | IPUSH   of int
             | IPOP
             | ISWAP
             | IGET    of int
             | IADD
             | ICALL   of label
             | ILAB    of label
             | IRETN

// Virtual machine

let private get = List.item

let rec private find l = function
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
      | (IPOP      :: ins,  _ :: st)      -> exec ins st
      | (ISWAP     :: ins,  y :: x :: st) -> exec ins (x :: y :: st)
      | (ICALL p   :: ILAB l :: _, st)    -> exec (find p prog) (l :: st)
      | (ILAB  l   :: ins,  st)           -> exec ins st
      | (IRETN     :: _,    l :: st)      -> exec (find l prog) st
  exec prog st

// Compiler

let mutable labelCounter = 0
let newLabel _ =
  let this = labelCounter
  labelCounter <- this + 1;
  this

let rec varpos x = function
  | []       -> failwith ("unbound: " + x)
  | y :: env -> if x = y then 0 else 1 + varpos x env

let rec comp fenv env = function
  | INT i           -> [IPUSH i]
  | ADD (e1, e2)    -> comp fenv env         e1 @
                       comp fenv ("" :: env) e2 @
                       [IADD]
  | VAR x           -> [IGET (varpos x env)]
  | CALL (f, e)     -> let lr = newLabel()  //auto generates label number
                       let lf = lookup f fenv //
                       comp fenv env e  @
                       [ICALL lf]       @
                       [ILAB lr]        @
                       [ISWAP]          @
                       [IPOP]

let compProg (funcs, e1) =
  let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs
  let rec compFuncs = function
    | []                   -> comp fenv [] e1 @
                              [IHALT]
    | (f, (x, e)) :: funcs -> let lf = lookup f fenv
                              compFuncs funcs     @
                              [ILAB lf]           @
                              comp fenv [""; x] e @
                              [ISWAP]             @
                              [IRETN]
  compFuncs funcs

// Example:
//
// Source program (concrete syntax):
//   func foo(x) = x + 42;
//   foo(8)
//
// Source program (AST):-->
//    [("foo", ("x", ADD (VAR "x", INT 42)))]   (this is funcs)
//    CALL("foo", INT 8)                        (this is e1)
//
// Function encivonmen (maps function names to labels):
//    [("foo", 13)]                             (this is fenv)
//
// Target program (actual output):
let ins = [
  IPUSH 8;
  ICALL 13;
  ILAB 14;
  ISWAP;
  IPOP;
  IHALT;
  ILAB 13;
  IGET 1;
  IPUSH 42;
  IADD;
  ISWAP;
  IRETN
]
//
// Target program (readable version)
//     IPUSH  8
//     ICALL  L13
//   L14:
//     ISWAP
//     IPOP
//     IHALT
//   L13:
//     IGET   1
//     IPUSH  42
//     IADD
//     ISWAP
//     IRETN
