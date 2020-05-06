module Lang

open Parser
open VM

// Interpreter
type 'a env = (varname list * 'a) list

let rec lookup x =
    function
    | [] -> failwith ("unbound: " + x)
    | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) =
    let rec eval env =
        function
        | INT i -> i
        | ADD (e1, e2) -> eval env e1 + eval env e2
        | VAR x -> lookup x env
        | NEG (e) -> -eval env e
        | EQ (e1, e2) -> if eval env e1 = eval env e2 then 1 else 0
        | SUB (e1, e2) -> eval env e1 - eval env e2
        | MUL (e1, e2) -> eval env e1 * eval env e2
        | DIV (e1, e2) -> eval env e1 / eval env e2
        | NEQ (e1, e2) -> if eval env e1 <> eval env e2 then 1 else 0
        | LT (e1, e2) -> if eval env e1 < eval env e2 then 1 else 0
        | LE (e1, e2) -> if eval env e1 <= eval env e2 then 1 else 0
        | GT (e1, e2) -> if eval env e1 > eval env e2 then 1 else 0
        | GE (e1, e2) -> if eval env e1 >= eval env e2 then 1 else 0
        | IF (e1, e2, e3) -> if eval env e1 = 1 then eval env e2 else eval env e3
        | AND (e1, e2) -> if eval env e1 = 1 && eval env e2 = 1 then 1 else 0
        | OR (e1, e2) -> if eval env e1 = 1 || eval env e2 = 1 then 1 else 0
        | LET (x, e1, e2) ->
            let v1 = eval env e1
            eval ((x, v1) :: env) e2
        | CALL (f, [ e ]) ->
            let v = eval env e
            let ([ x ], body) = lookup f funcs
            eval [ (x, v) ] body
        | CALL (f, [ e1; e2 ]) ->
            let v1 = eval env e1
            let v2 = eval env e2
            let ([ x1; x2 ], body) = lookup f funcs
            eval [ (x1, v1); (x2, v2) ] body

    eval [] e

// Compiler
let mutable labelCounter = 0

let newLabel _ =
    let this = labelCounter
    labelCounter <- this + 1
    this

let rec varpos x =
    function
    | [] -> failwith ("unbound: " + x)
    | y :: env -> if x = y then 0 else 1 + varpos x env

let rec comp fenv env =
    function
    | INT i -> [ IPUSH i ]
    | ADD (e1, e2) ->
        comp fenv env e1
        @ comp fenv ("" :: env) e2
        @ [ IADD ]
    | VAR x -> [ IGET(varpos x env) ]
    | LET (x, e1, e2) ->
        comp fenv env e1
        @ comp fenv (x :: env) e2
        @ [ ISWAP ]
        @ [ IPOP ]
    | EQ (e1, e2) ->
        comp fenv env e1
        @ comp fenv ("" :: env) e2
        @ [ IEQ ]
    | IF (e1, e2, e3) ->
        let l2 = newLabel ()
        let le = newLabel ()
        comp fenv env e1
        @ [ IJMPIF l2 ]
        @ comp fenv env e3
        @ [ IJMP le ]
        @ [ ILAB l2 ]
        @ comp fenv env e2
        @ [ ILAB le ]
    | CALL (f,  e ) ->
        let lr = newLabel ()
        let lf = lookup f fenv
        comp fenv env e
        @ [ ICALL lf ]
        @ [ ILAB lr ]
        @ [ ISWAP ]
        @ [ IPOP ]
    | SUB (_, _) -> failwith "Not Implemented"
    | NEG (_) -> failwith "Not Implemented"
    | MUL (_, _) -> failwith "Not Implemented"
    | DIV (_, _) -> failwith "Not Implemented"
    | NEQ (_, _) -> failwith "Not Implemented"
    | LT (_, _) -> failwith "Not Implemented"
    | LE (_, _) -> failwith "Not Implemented"
    | GT (_, _) -> failwith "Not Implemented"
    | GE (_, _) -> failwith "Not Implemented"
    | AND (_, _) -> failwith "Not Implemented"
    | OR (_, _) -> failwith "Not Implemented"


let compProg (funcs, e1) =
    let fenv =
        List.map (fun (f, _) -> (f, newLabel ())) funcs

    let rec compFuncs =
        function
        | [] -> comp fenv [] e1 @ [ IHALT ]
        | (f, ( x , e)) :: funcs ->
            let lf = lookup f fenv
            compFuncs funcs
            @ [ ILAB lf ]
            @ comp fenv [ ""; x ] e
            @ [ ISWAP ]
            @ [ IRETN ]

    compFuncs funcs


