module Final2

open Parser
open VM

(* type varname  = string
type funcname = string
type exp      = | INT     of int                    // i
                | ADD     of exp * exp            // e1 + e2
                | VAR     of varname     
                | SUB     of exp * exp    
                | DIV     of exp * exp  
               // | MUL     of exp * exp 
                | EQ     of exp * exp 
                | LT     of exp * exp 
                | AND     of exp * exp
                | OR    of exp * exp
              //  | MUL     of exp * exp
                | MUL     of exp * exp      // x
                | CALL    of funcname * exp list                   

type func     = funcname * (varname list * exp)  *)    // func f ( x ) = e

//let s = parseProgFromString "1+2"

type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) = 
  let rec eval env = function
    | INT i              -> i
    | ADD (e1, e2)       -> eval env e1 + eval env e2
    | SUB (e1, e2)       -> eval env e1 - eval env e2
    | DIV (e1, e2)       -> eval env e1 / eval env e2
    | MUL (e1, e2)       -> eval env e1 * eval env e2
    | VAR x              -> lookup x env
    | LET (x, e1, e2)    -> let v1 = eval env e1
                            eval ((x, v1) :: env) e2 
    | EQ (e1, e2)        -> if eval env e1 =  eval env e2 then 1 else 0
    | NEQ (e1, e2)       -> if eval env e1 <> eval env e2 then 1 else 0
    | LT (e1, e2)        -> if eval env e1 <  eval env e2 then 1 else 0
    | LE (e1, e2)        -> if eval env e1 <= eval env e2 then 1 else 0
    | GT (e1, e2)        -> if eval env e1 >  eval env e2 then 1 else 0
    | GE (e1, e2)        -> if eval env e1 >= eval env e2 then 1 else 0
    | IF (e1, e2, e3)    -> if eval env e1 = 1 then eval env e2 else eval env e3                    //tjek op 
    | OR (e1, e2)        -> if eval env e1 = 1 then 1 else eval env e2                      //tjek op
    | AND (e1, e2)       -> if eval env e1 = 1 then (if eval env e2 = 1 then 1 else 0) else 0                            //tjek op  if e1 then e2 else 0
    | CALL (f, [e1])     -> let v = eval env e1                              // evaluates the input to the function, which can be a sequence of exps (fx ADD(INT, INT)) 
                            let ([x], body) = lookup f funcs                // look for the function in the function env. Fx lookup "foo" in [("foo", ("x", ADD (VAR "x", INT 42)))] and add the variable name "x" to x and ADD (VAR "x", INT 42) to body
                            eval [(x, v)] body     
    | CALL (f, [e1; e2]) -> let v1 = eval env e1  
                            let v2 = eval env e2 
                            let ([x; k], body) = lookup f funcs
                            eval [(x, v1); (k, v2)] body       
    | CALL (f, es)       -> let rec bind xs es =
                                match (xs, es) with
                                  |([], []) -> []
                                  |(x :: xs, e :: es) -> let v = eval env e 
                                                         (x, v) :: bind xs es
                            let (xs, body) = lookup f funcs
                            eval (bind xs es) body                                          
  eval [] e                                              
//evalProg s
//let run s = evalProg (parseProgFromString s)
//evalProg([], OR(EQ(INT 2, INT 1), EQ(INT 20, INT 20)))


//evalProg ([("foo", (["x"; "y"; "z"], ADD (MUL(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 6; INT 5; INT 2]))

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


let private get = List.item

let rec private find l = function
  | (ILAB l' :: ins) -> if l = l' then ins else find l ins
  | (_       :: ins) -> find l ins

 //[IPUSH 10; IPUSH 42; IPUSH 11; ICALL 6; ILAB 7; ISWAP; IPOP; ISWAP; IPOP; IHALT; ILAB 6; IGET 1; IGET 3; IADD; IGET 4; IADD; ISWAP; IRETN]

let execProg prog st =
  let rec exec ins st =
    match (ins, st) with
      | ([],                v :: _)       -> v
      | (IHALT     :: _,    v :: _)       -> v                             //63, 10
      | (IPUSH i   :: ins,  st)           -> exec ins (i :: st)             //11, 42, 10    
      | (IGET p    :: ins,  st)           -> exec ins (get p st :: st)      //10, 53, 7, 11, 42, 10   //42, 11, 7, 11, 42, 10   //11, 7, 11, 42, 10   
      | (IADD      :: ins,  y :: x :: st) -> exec ins (x + y :: st)         //63, 7, 11, 42, 10 //53, 7, 11, 42, 10 
      | (IPOP      :: ins,  _ :: st)      -> exec ins st                    
      | (ISWAP     :: ins,  y :: x :: st) -> exec ins (x :: y :: st)        //7, 63, 11, 42, 10   
      | (ICALL p   :: ILAB l :: _, st)    -> exec (find p prog) (l :: st)   //7, 11, 42, 10
      | (ILAB  l   :: ins,  st)           -> exec ins st
      | (IRETN     :: _,    l :: st)      -> exec (find l prog) st          //63, 11, 42, 10   
  exec prog st


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
  | CALL (f, es)        -> //let sps = [ISWAP] @ [IPOP]
                           let rec bind es =
                             match es with
                               | []       -> []
                               | e::es    -> comp fenv env e @ bind es                                                               
                           let lr = newLabel()                      
                           let lf = lookup f fenv
                           bind es          @                                         
                           [ICALL lf]       @           
                           [ILAB lr]        @                                             
                           [ISWAP]          @          
                           [IPOP]           @   //needs as many swaps and pops as there elements in es
                           [ISWAP]          @             
                           [IPOP]                
  

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
    | (f, (x::xs, e)) :: funcs     -> let lf = lookup f fenv   
                                      compFuncs funcs          @    
                                      [ILAB lf]                @    
                                      comp fenv (""::x::xs) e  @    
                                      [ISWAP]                  @    
                                      [IRETN]                       
  compFuncs funcs                                          


//let exL2 = compProg ([("foo", (["x"; "y"; "z"], ADD (ADD(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 10; INT 42; INT 11]))  

//let exL1 = compProg ([("foo", (["x"], ADD (VAR "x", INT 42)))], CALL("foo", [INT 8]))          //<- works

//let exL = compProg ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 10; INT 42]))  
         

 
//execProg exL2 []