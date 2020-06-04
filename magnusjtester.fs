
type varname  = string
type funcname = string
type exp      = | INT     of int                  
                | ADD     of exp * exp            
                | SUB     of exp * exp            
                | NEG     of exp               
                | MUL     of exp * exp         
                | DIV     of exp * exp           
                | EQ      of exp * exp          
                | NEQ     of exp * exp          
                | LT      of exp * exp           
                | LE      of exp * exp         
                | GT      of exp * exp          
                | GE      of exp * exp          
                | VAR     of varname             
                | LET     of varname * exp * exp  
                | IF      of exp * exp * exp      
                | AND     of exp * exp           
                | OR      of exp * exp                          
                | CALL    of funcname * exp list  
                                 
type func     = funcname * (varname list * exp)      


type 'a env = (varname * 'a) list
let rec lookup x = function
  | []            -> failwith ("unbound: " + x)
  | (y, w) :: env -> if x = y then w else lookup x env
 
  
let evalProg (funcs, e) =             
  let rec eval env = function
    | INT i              -> i
    | NEG e              -> 0 - eval env e  
    | ADD (e1, e2)       -> eval env e1 + eval env e2
    | SUB (e1, e2)       -> eval env e1 - eval env e2
    | DIV (e1, e2)       -> eval env e1 / eval env e2
    | MUL (e1, e2)       -> eval env e1 * eval env e2
    | VAR x              -> lookup x env
    | LET (x, e1, e2)    -> let v1 = eval env e1         //local variable
                            eval ((x, v1) :: env) e2 
    | EQ (e1, e2)        -> if eval env e1 =  eval env e2 then 1 else 0
    | NEQ (e1, e2)       -> if eval env e1 <> eval env e2 then 1 else 0
    | LT (e1, e2)        -> if eval env e1 <  eval env e2 then 1 else 0
    | LE (e1, e2)        -> if eval env e1 <= eval env e2 then 1 else 0
    | GT (e1, e2)        -> if eval env e1 >  eval env e2 then 1 else 0
    | GE (e1, e2)        -> if eval env e1 >= eval env e2 then 1 else 0
    | IF (e1, e2, e3)    -> if eval env e1 = 1 then eval env e2 else eval env e3                     
    | OR (e1, e2)        -> if eval env e1 = 1 then 1 else eval env e2                              
    | AND (e1, e2)       -> if eval env e1 = 1 then (if eval env e2 = 1 then 1 else 0) else 0  // if eval env e1 = 1 then eval e2 else 0                            //tjek op  if e1 then e2 else 0     
    | CALL (f, es)       -> let rec bind xs es =   
                                match (xs, es) with
                                  |([], []) -> []   
                                  |(x :: xs, e :: es) -> let v = eval env e   
                                                         (x, v) :: bind xs es 
                            let (xs, body) = lookup f funcs  
                            eval (bind xs es) body                     
  eval [] e   

let fourExp = ([("foo", (["x"; "y"; "z"; "q"], ADD (ADD(VAR "x", VAR "y"), ADD(VAR "z", VAR "q"))))], CALL("foo", [INT 6; INT 5; INT 2; INT 100]))
let thrExp = ([("foo", (["x"; "y"; "z"], ADD (ADD(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 6; INT 5; INT 2]))
let twoExp = ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))], CALL("foo", [INT 6; INT 5])) // <
let oneExp = ([("foo", (["x"], ADD (VAR "x", INT 1)))], CALL("foo", [INT 6]))

evalProg ([], LET("x", INT 6, VAR "x"))

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
let rec private find l = function           //find label and return all the ins after the label
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
      | (IJMPIF l  :: _,    1 :: st)      -> exec (find l prog) st   //fjerner 1 fra stack
      | (IJMPIF l  :: ins,  _ :: st)      -> exec ins st
      | (ICALL p   :: ILAB l :: _, st)    -> exec (find p prog) (l :: st) //hopper til ILAB p og tilføjer return label til toppen af st
      | (ILAB  l   :: ins,  st)           -> exec ins st
      | (IRETN     :: _,    l :: st)      -> exec (find l prog) st  //hopper til ILAB l og sletter l fra st
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

let rec comp fenv env = function                              
  | INT i               -> [IPUSH i]

  | ADD (e1, e2)        -> comp fenv env         e1 @  
                           comp fenv ("" :: env) e2 @ 
                           [IADD]  
  | NEG e               -> [IPUSH 0]                @
                           comp fenv ("":: env) e   @  // "" er fordi e2 skal GETTE +1 på stacken ellers GETTER den den forreste som er 0
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
  | AND (e1, e2)        -> let l2 = newLabel()        //kan også laves som MUL
                           let le = newLabel()
                           comp fenv env e1  @        //resulterer i 1 eller 0 på toppen af stack
                           [IJMPIF l2]  @             // hvis top element = 1 så hopper den til l2. fjerner top af stack.
                           [IPUSH 0] @                // hvis ikke er udtrykket falsk og den pusher 0 på top af stack og hopper til le
                           [IJMP le]    @             
                           [ILAB l2]    @
                           comp fenv ("" :: env) e2  @
                           [ILAB le]  
  | OR (e1, e2)         -> let l2 = newLabel()          //kan også laves som MUL
                           let le = newLabel()
                           comp fenv env e1  @          //resulterer i 1 eller 0 på toppen af stack
                           [IJMPIF l2]  @               //hvis toppen af stack er 1 så hop til l2, fordi så er udtrykket sandt
                           comp fenv ("" :: env) e2  @  // resulterer i 1 eller 0 på topppen af stack
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
                           comp fenv env e1  @  //resulterer i 1 eller 0
                           [IJMPIF l2]  @       //hvis 1 hop til l2
                           comp fenv env e3 @   //hvis 0 comp e3
                           [IJMP le]    @
                           [ILAB l2]    @
                           comp fenv env e2  @  //hvis 1 comp e2
                           [ILAB le]                                      
  | CALL (f, es)        -> 
                           let rec bind (es, spl) lr lf = 
                             match (es, spl) with
                               | ([], spl)     -> [] @ [ICALL lf] @ [ILAB lr] @ spl   //swap og pop fjerner 'es'
                               | (e::es, spl)  -> comp fenv env e @ bind (es, [ISWAP] @ [IPOP] @ spl) lr lf                                                                         
                           let lr = newLabel()        //return label              
                           let lf = lookup f fenv     //function label
                           bind (es, []) lr lf                 


let compProg (funcs, e1) = // compiles functions 
  let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs  // functions are assigned labels 
  let rec compFuncs = function
    | []                           -> comp fenv [] e1 @        //compiles the function arguments (inputs) 
                                      [IHALT]                  
    | (f, (x::xs, e)) :: funcs     -> let lf = lookup f fenv        //lf = function label from fenv
                                      compFuncs funcs          @  
                                      [ILAB lf]                @  
                                      comp fenv (""::x::xs) e  @   //compiles function body e with the function variables as environment. "" fordi ICALL putter et label forrest så 
                                      [ISWAP]                  @    
                                      [IRETN]                       
  compFuncs funcs       


compProg ([], GT(INT 1, INT 2))

let anda = compProg ([], IF(OR(EQ(INT 2, INT 2), EQ(INT 3, INT 4)), INT 4, INT 6))
let neg = compProg ([], NEG(INT 15))

let eq = compProg ([], IF(EQ(INT 1, INT 2), INT 4, INT 2))

let exL2 = compProg ([("foo", (["x"; "y"; "z"], MUL (ADD(VAR "x", VAR "y"), VAR "z")))], CALL("foo", [INT 10; INT 42; INT 11]))  

let exL1 = compProg ([("foo", (["x"], ADD (VAR "x", INT 42)))], CALL("foo", [INT 8]))          //<- works

let exL = compProg ([("foo", (["x"; "y"], ADD (VAR "x", VAR "y")))],CALL("foo", [INT 10; INT 42]))  

let subtest = compProg ([], NEG (INT 1))

let neqtest = compProg ([], NEQ(INT 2, INT 2))
 
let addtest = compProg ([], LET("x", INT 2, LET("y", INT 3, ADD (VAR "x", VAR "y")) ))

let andtest = compProg ([], AND(EQ(INT 1, INT 1), EQ(INT 2, INT 2)))
execProg exL []



//   [IPUSH 10; IPUSH 42; ICALL 14; ILAB 15; ISWAP; IPOP; ISWAP; IPOP; IHALT;
//   ILAB 14; IGET 1; IGET 3; IADD; ISWAP; IRETN]
// 10
// 42 10
// 15 42 10
// 42 15 42 10
// 10 42 15 42 10
// 52 15 42 10
// 15 52 42 10
// 52 42 10
// 42 52 10
// 52 10
// 10 52
// 52