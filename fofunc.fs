//module Fofunc
//open Parser 
//open VM

//let Parser = #r "parser.dll"

// AST of a language with functions


type varname  = string
type funcname = string
type exp      = | INT     of int             // i
                | ADD     of exp * exp       // e1 + e2
                | VAR     of varname         // x
                | CALL    of funcname * exp list  // f ( e )                
type func     = funcname * (varname list * exp)   // func f ( x ) = e


// Interpreter

(* type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) =
  let rec eval env = function
    | INT i           -> i
    | ADD (e1, e2)    -> eval env e1 + eval env e2
    | VAR x           -> lookup x env
    | CALL (f, e)     -> let v = eval env e              // evaluates the input to the function, which can be a sequence of exps (fx ADD(INT, INT)) 
                         let (x, body) = lookup f funcs  // look for the function in the function env. Fx lookup "foo" in [("foo", ("x", ADD (VAR "x", INT 42)))] and add the variable name "x" to x and ADD (VAR "x", INT 42) to body
                         eval [(x, v)] body              // evaluate with a small environment only containing the "x" bound to v which fx evaluates to 8 in the below example. With this env it evaluates the body which is fx ADD(VAR "x", INT 42). This evaluation will lookup the "x" in the env and find the value 8 and add it to 42.
  eval [] e                                              // ??

evalProg ([("foo", ("x", ADD (VAR "x", INT 42)))], CALL("foo", ADD(INT 4, INT 4)))  *)



// Interpreter

type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) = 
  let rec eval env = function
    | INT i              -> i
    | ADD (e1, e2)       -> eval env e1 + eval env e2
    | VAR x              -> lookup x env
    | CALL (f, [e1])     -> let v = eval env e1                              // evaluates the input to the function, which can be a sequence of exps (fx ADD(INT, INT)) 
                            let ([x], body) = lookup f funcs                // look for the function in the function env. Fx lookup "foo" in [("foo", ("x", ADD (VAR "x", INT 42)))] and add the variable name "x" to x and ADD (VAR "x", INT 42) to body
                            eval [(x, v)] body     
    | CALL (f, [e1; e2]) -> let v1 = eval env e1  
                            let v2 = eval env e2 
                            let ([x; k], body) = lookup f funcs
                            eval [(x, v1); (k, v2)] body                       
  eval [] e                                              

//evalProg ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 6; INT 5])) // <-- works

// foo(x, y) = x + y 
// foo(4, 8) = 4 + 8



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

// [IPUSH 8; IPUSH 42; ICALL 0; ILAB 1; I SWAP; IPOP; IHALT; ILAB 0; IGET 1; IGET 3; IADD; ISWAP; IRETN]

let execProg prog st =
  let rec exec ins st =
    match (ins, st) with
      | ([],                v :: _)       -> v
      | (IHALT     :: _,    v :: _)       -> v                             
      | (IPUSH i   :: ins,  st)           -> exec ins (i :: st)       //8   // 42; 8     
      | (IGET p    :: ins,  st)           -> exec ins (get p st :: st)      // 42; 1; 42; 8  //8; 42; 1; 42; 8
      | (IADD      :: ins,  y :: x :: st) -> exec ins (x + y :: st)         // 50; 1; 42; 8
      | (IPOP      :: ins,  _ :: st)      -> exec ins st                    // 50; 8
      | (ISWAP     :: ins,  y :: x :: st) -> exec ins (x :: y :: st)        // 1; 50; 42; 8   //42; 50; 8
      | (ICALL p   :: ILAB l :: _, st)    -> exec (find p prog) (l :: st)   // 1; 42; 8
      | (ILAB  l   :: ins,  st)           -> exec ins st
      | (IRETN     :: _,    l :: st)      -> exec (find l prog) st          //50; 42; 8
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



let rec comp fenv env = function                       // compiles function arg and function calc
  | INT i               -> [IPUSH i]
  | ADD (e1, e2)        -> comp fenv env         e1 @  // comp fenv ["", "x"] VAR "X" ->  IGET 1
                           comp fenv ("" :: env) e2 @  // comp fenv ""::""::"x"::[] VAR "Y" ->
                           [IADD]                      // [IGET1, IPUSH 42, IADD]
  | VAR x               -> [IGET (varpos x env)]
  | CALL (f, [a1])      -> let lr = newLabel()         // lr = 1 
                           let lf = lookup f fenv      // lf = 0
                           comp fenv env a1  @         // comp fenv [] INT 8 -> [IPUSH 8]
                           [ICALL lf]        @         // [ICALL 0]
                           [ILAB lr]         @         // [ILAB 1]                       
                           [ISWAP]           @         // [ISAWP]
                           [IPOP]                      // [IPOP] -> [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP]
  | CALL (f, [a1; a2])  -> let lr = newLabel()         // lr = 1 
                           let lf = lookup f fenv      // lf = 0
                           comp fenv env a1 @          //[IPUSH 8]  
                           comp fenv env a2 @          //[IPUSH 8; IPUSH 42]
                           [ICALL lf]       @          //[ICALL 0]  
                           [ILAB lr]        @          //[ILAB 1]                        
                           [ISWAP]          @          
                           [IPOP]           @
                           [ISWAP]          @             
                           [IPOP]                   //[IPUSH 8; IPUSH 42; ICALL 0; ILAB 1; I]   
  

let compProg (funcs, e1) = // compiles functions
  let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs
  let rec compFuncs = function
    | []                           -> comp fenv [] e1 @             // compiles the argument that is passed into the function, which also calls the function body with ICALL 
                                      [IHALT]                       // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT] <- list with argument on top of list. swap and pops to remove the return call?
    | (f, ([x], e)) :: funcs       -> let lf = lookup f fenv        // lf = 0
                                      compFuncs funcs     @         // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT]
                                      [ILAB lf]           @         // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT, ILAB 0]
                                      comp fenv [""; x] e @         // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT, ILAB 0, IGET1, IPUSH 42, IADD]  - compiles instructions for the body of the function (the calculations)
                                      [ISWAP]             @         // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT ILAB 0, IGET1, IPUSH 42, IADD, ISWAP]
                                      [IRETN]  
    | (f, ([x1; x2], e)) :: funcs  -> let lf = lookup f fenv   
                                      compFuncs funcs          @    //[IPUSH 8; IPUSH 42; ICALL 0; ILAB 1; I SWAP; IPOP; IHALT;
                                      [ILAB lf]                @    //[ILAB 0]
                                      comp fenv [""; x1; x2] e @    //
                                      [ISWAP]                  @    
                                      [IRETN]                       //[IGET 1; IGET 3; IADD; ISWAP; IRETN]
  compFuncs funcs                                          


//[IPUSH 8; IPUSH 42; ICALL; ILAB 1; I SWAP; IPOP; IHALT; ILAB 0; IGET 1; IGET 3; IADD; ISWAP; IRETN]

compProg ([("foo", (["x"], ADD (VAR "x", INT 42)))], CALL("foo", [INT 8]))          //<- works
let exL = compProg ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 10; INT 42]))          

execProg exL []

// It compiles the expression (e) into a push of the function argument (in this case PUSH 8). 
// Appends a call inst to the function calculation, aswell as a return label, to return to 
// when the function calculation is done (in this case ICALL 0 + ILAB 1). The swap and pop is appended 
// to get rid of the function argument. Appends the ILAB for the function body calculation 
// and appends instrunctions for the functions body after that ILAB. After the body is executed, 
// the value of the body is on top of the stack, and the return label is on position 1, therefore the ISWAP. 

(* 
let rec comp fenv env = function
  | INT i               -> [IPUSH i]
  | ADD (e1, e2)        -> comp fenv env         e1 @  //comp fenv ["", "x"] VAR "X" ->  IGET 1
                           comp fenv ("" :: env) e2 @  //comp fenv ["", "", "x"] INT 42  -> IPUSH 42
                           [IADD]                      // [IGET1, IPUSH 42, IADD]
  | VAR x               -> [IGET (varpos x env)]
  | CALL (f, [a1])      -> let lr = newLabel()    //lr = 1 
                           let lf = lookup f fenv // lf = 0
                           comp fenv env a1  @     // comp fenv [] INT 8 -> [IPUSH 8]
                           [ICALL lf]       @     // [ICALL 0]
                           [ILAB lr]        @     // [ILAB 1]                       
                           [ISWAP]          @     // [ISAWP]
                           [IPOP]                 // [IPOP]                         [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP]

  

let compProg (funcs, e1) =
  let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs
  let rec compFuncs = function
    | []                      -> comp fenv [] e1 @     // compiles the argument that is passed into the function, which also calles the function body with ICALL 
                                 [IHALT]               // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT]  
    | (f, (x, [a1])) :: funcs -> let lf = lookup f fenv  //lf = 0
                                 compFuncs funcs     @   //[IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT]
                                 [ILAB lf]           @   //[IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT, ILAB 0]
                                 comp fenv [""; x] a1 @   //[IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT, ILAB 0, IGET1, IPUSH 42, IADD]  - compiles instructions for the body of the function (the calculations)
                                 [ISWAP]             @   //[IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT ILAB 0, IGET1, IPUSH 42, IADD, ISWAP]
                                 [IRETN]                 //[IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT, ILAB 0, IGET 1, IPUSH 42, IADD, ISWAP, IRETN]
  compFuncs funcs                                        //[IPUSH 8; ICALL 0; ILAB 1; ISWAP; IPOP; IHALT; ILAB 0; IGET 1; IPUSH 42; IADD; ISWAP; IRETN]


compProg ([("foo", ("x", [ADD (VAR "x", INT 42)]))], CALL("foo", [INT 8]))  *)



(* 
[IPUSH 8;  8
ICALL 0;   
ILAB 1;    1 8
ISWAP;     8 50
IPOP;      50 
IHALT; 
ILAB 0;    
IGET 1;    8 1 8
IPUSH 42;  42 8 1 8
IADD;      50 1 8
ISWAP;     1 50 8
IRETN    
 *)


(*    | CALL (f, [e1])     -> let lr = newLabel()
                           let lf = lookup f fenv
                           comp fenv env e1 @
                           [ICALL lf]       @
                           [ILAB lr]        @
                           [ISWAP]          @
                           [IPOP] *)

(*  | (f, (x, [e1])) :: funcs -> let lf = lookup f fenv 
                                 compFuncs funcs      @
                                 [ILAB lf]            @
                                 comp fenv [""; x] e1 @
                                 [ISWAP]
                                 [IRETN] *)

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

