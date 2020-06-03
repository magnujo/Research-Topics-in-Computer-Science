
//open Parser
//open VM 


type varname  = string
type funcname = string
type exp      = | INT     of int                  // i
                | ADD     of exp * exp            // e1 + e2
                | SUB     of exp * exp            // e1 - e2
                | NEG     of exp                  // - e
                | MUL     of exp * exp            // e1 * e2
                | DIV     of exp * exp            // e1 / e2
                | EQ      of exp * exp            // e1 == e2
                | NEQ     of exp * exp            // e1 != e2
                | LT      of exp * exp            // e1 < e2
                | LE      of exp * exp            // e1 <= e2
                | GT      of exp * exp            // e1 > e2
                | GE      of exp * exp            // e1 >= e2
                | VAR     of varname              // x
                | LET     of varname * exp * exp  // let x = e1 in e2
                | IF      of exp * exp * exp      // if e1 then e2 else e3
                | AND     of exp * exp            // e1 && e1
                | OR      of exp * exp            // e1 || e1                
                | CALL    of funcname * exp list  // f ( e1, ..., en )
                                 
type func     = funcname * (varname list * exp)      // func f ( x ) = e


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
    | OR (e1, e2)        -> if eval env e1 = 1 then 1 else eval env e2                              //tjek op
    | AND (e1, e2)       -> if eval env e1 = 1 then (if eval env e2 = 1 then 1 else 0) else 0                            //tjek op  if e1 then e2 else 0     
    | CALL (f, es)       -> let rec bind xs es =
                                match (xs, es) with
                                  |([], []) -> []
                                  |(x :: xs, e :: es) -> let v = eval env e 
                                                         (x, v) :: bind xs es
                            let (xs, body) = lookup f funcs
                            eval (bind xs es) body                                          
  eval [] e   

(* let fourExp = ([("foo", (["x"; "y"; "z"; "q"], ADD (ADD(VAR "x", VAR "y"), ADD(VAR "z", VAR "q"))))], CALL("foo", [INT 6; INT 5; INT 2; INT 100]))
let thrExp = ([("foo", (["x"; "y"; "z"], ADD (ADD(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 6; INT 5; INT 2]))
let twoExp = ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 6; INT 5])) // <
let oneExp = ([("foo", (["x"], ADD (VAR "x", INT 1)))], CALL("foo", [INT 6]))

evalProg fourExp *)

type label = int
type inst  = | IHALT
             | IPUSH   of int
             | IPOP
             | ISWAP
             | IGET    of int
             | IADD
             | ISUB
             | IMUL
             | IDIV             
             | IEQ
             | ILT
             | ILE
             | IJMP    of label
             | IJMPIF  of label
             | ICALL   of label
             | ILAB    of label
             | IRETN

// Virtual machine

let private get = List.item
let rec private find l = function
  | []               -> failwith "error execuing code"
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
      | (ISUB      :: ins,  y :: x :: st) -> exec ins (x - y :: st)
      | (IMUL      :: ins,  y :: x :: st) -> exec ins (x * y :: st)
      | (IDIV      :: ins,  y :: x :: st) -> exec ins (x / y :: st)
      | (IEQ       :: ins,  y :: x :: st) ->
        if x = y then exec ins (1 :: st) else exec ins (0 :: st)
      | (ILT       :: ins,  y :: x :: st) ->
        if x < y then exec ins (1 :: st) else exec ins (0 :: st)
      | (ILE       :: ins,  y :: x :: st) ->
        if x <= y then exec ins (1 :: st) else exec ins (0 :: st)
      | (IPOP      :: ins,  _ :: st)      -> exec ins st
      | (ISWAP     :: ins,  y :: x :: st) -> exec ins (x :: y :: st)
      | (IJMP l    :: _,    st)           -> exec (find l prog) st
      | (IJMPIF l  :: _,    1 :: st)      -> exec (find l prog) st
      | (IJMPIF l  :: ins,  _ :: st)      -> exec ins st
      | (ICALL p   :: ILAB l :: _, st)    -> exec (find p prog) (l :: st)
      | (ILAB  l   :: ins,  st)           -> exec ins st
      | (IRETN     :: _,    l :: st)      -> exec (find l prog) st
      | _                                 -> failwith "error executing code"
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
                           [IADD]  
  | NEG e               -> [IPUSH 0]                @
                           comp fenv ("":: env) e   @
                           [ISUB]                
  | SUB (e1, e2)        -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [ISUB]      
  | MUL (e1, e2)        -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [IMUL]   
  | DIV (e1, e2)        -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [IDIV]   
  | GT (e1, e2)         -> comp fenv env         e2 @
                           comp fenv ("" :: env) e1 @
                           [ILT]                                                                                         
  | VAR x               -> [IGET (varpos x env)]
  | EQ (e1, e2)         -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [IEQ]
  | NEQ (e1, e2)        -> [IPUSH 1] @
                           comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [IEQ] @      
                           [ISUB]                  
  | LT (e1, e2)         -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [ILT] 
  | LE (e1, e2)         -> comp fenv env         e1 @
                           comp fenv ("" :: env) e2 @
                           [ILE]           
  | GE (e1, e2)         -> comp fenv env         e2 @
                           comp fenv ("" :: env) e1 @
                           [ILE]                                                              
  | AND (e1, e2)        -> let l2 = newLabel()          //kan også laves som MUL
                           let le = newLabel()
                           comp fenv env e1  @        //resulterer i 1 eller 0 på toppen af stack
                           [IJMPIF l2]  @             // hvis top element = 1 så hopper den. fjerner top af stack.
                           [IPUSH 0] @
                           [IJMP le]    @
                           [ILAB l2]    @
                           comp fenv ("" :: env) e2  @
                           [ILAB le]  
  | OR (e1, e2)         -> let l2 = newLabel()          //kan også laves som MUL
                           let le = newLabel()
                           comp fenv env e1  @
                           [IJMPIF l2]  @
                           comp fenv ("" :: env) e2  @
                           [IJMP le]    @
                           [ILAB l2]    @
                           [IPUSH 1]    @
                           [ILAB le]                               
  | LET (x, e1, e2)     -> comp fenv env        e1 @
                           comp fenv (x :: env) e2 @
                           [ISWAP]            @
                           [IPOP]        
  | IF (e1, e2, e3)     -> let l2 = newLabel()
                           let le = newLabel()
                           comp fenv env e1  @
                           [IJMPIF l2]  @
                           comp fenv env e3 @
                           [IJMP le]    @
                           [ILAB l2]    @
                           comp fenv env e2  @
                           [ILAB le]                                      
  | CALL (f, es)        -> 
                           let rec bind (es, spl) lr lf =
                             match (es, spl) with
                               | ([], spl)       -> [] @ [ICALL lf] @ [ILAB lr] @ spl
                               | (e::es, spl)    -> comp fenv env e @ bind (es, [ISWAP]@[IPOP]@spl) lr lf                                                                                   
                           let lr = newLabel()                      
                           let lf = lookup f fenv
                           bind (es, []) lr lf                 
  

let compProg (funcs, e1) = // compiles functions
  let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs
  let rec compFuncs = function
    | []                           -> comp fenv [] e1 @             // compiles the argument that is passed into the function, which also calls the function body with ICALL 
                                      [IHALT]                       // [IPUSH 8, ICALL 0, ILAB 1, ISWAP, IPOP, IHALT] <- list with argument on top of list. swap and pops to remove the return call?                  
    | (f, (x::xs, e)) :: funcs     -> let lf = lookup f fenv   
                                      compFuncs funcs          @    
                                      [ILAB lf]                @    
                                      comp fenv (""::x::xs) e  @    
                                      [ISWAP]                  @    
                                      [IRETN]                       
  compFuncs funcs       


compProg ([], GT(INT 1, INT 2))

let anda = compProg ([], IF(OR(EQ(INT 2, INT 2), EQ(INT 3, INT 4)), INT 4, INT 6))
let neg = compProg ([], NEG(INT 15))

let eq = compProg ([], IF(EQ(INT 1, INT 2), INT 4, INT 2))

let exL2 = compProg ([("foo", (["x"; "y"; "z"], MUL (ADD(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 10; INT 42; INT 11]))  

let exL1 = compProg ([("foo", (["x"], ADD (VAR "x", INT 42)))], CALL("foo", [INT 8]))          //<- works

let exL = compProg ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 10; INT 42]))  
        

 
execProg exL2 []