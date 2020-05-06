module Finaltest

//open Parser
//open VM 


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
    | IF (e1, e2, e3)    -> if eval env e1 = 1 then eval env e2 else eval env e3                    
    | OR (e1, e2)        -> if eval env e1 = 1 then 1 else eval env e2                              
    | AND (e1, e2)       -> if eval env e1 = 1 then (if eval env e2 = 1 then 1 else 0) else 0                                 
    | CALL (f, es)       -> let rec bind xs es =
                                match (xs, es) with
                                  |([], []) -> []
                                  |(x :: xs, e :: es) -> let v = eval env e 
                                                         (x, v) :: bind xs es
                            let (xs, body) = lookup f funcs
                            eval (bind xs es) body                                          
  eval [] e                                              

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
