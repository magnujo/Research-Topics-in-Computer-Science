module Test

open Parser



type 'a env = (varname list * 'a) list

let rec lookup x =
    function
    | [] -> failwith ("unbound: " + x)
    | (y, w) :: env -> if x = y then w else lookup x env

let evalProg (funcs, e) =
    let rec eval env =
        function
        | INT i           -> i
        | NEG (e1)        -> - eval env e1
        | ADD (e1, e2)    -> eval env e1 + eval env e2
        | SUB (e1, e2)    -> eval env e1 - eval env e2
        | DIV (e1, e2)    -> eval env e1 / eval env e2
        | MUL (e1, e2)    -> eval env e1 * eval env e2
        | OR (e1, e2)     -> if eval env e1 = 1 then 1 else if  eval env e2 = 1 then 1 else 0
        | EQ (e1, e2)     -> if eval env e1 = eval env e2 then 1 else 0
        | NEQ (e1, e2)    -> if eval env e1 <> eval env e2 then 1 else 0
        | LT (e1, e2)     -> if eval env e1 < eval env e2 then 1 else 0
        | LE (e1, e2)     -> if eval env e1 <= eval env e2 then 1 else 0
        | GT (e1, e2)     -> if eval env e1 > eval env e2 then 1 else 0
        | GE (e1, e2)     -> if eval env e1 >= eval env e2 then 1 else 0
        | IF (e1,e2,e3)   -> if eval env e1 = 1 then eval env e2 else eval env e3
        | LET (x, e1, e2) -> let v1 = eval env e1
                             eval ((x, v1) :: env) e2
        | VAR x           -> lookup x env



    eval [] e
let run s = evalProg (parseProgFromString s)
let Parse s = parseProgFromString s

let ParseExp s = parseExpFromString s
