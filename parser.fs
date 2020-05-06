module Parser

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
                | FAIL                            // fail
                | CATCH   of exp * exp            // e1 catch e2
                | SEQ     of exp * exp            // e1 ; e2
type func     = funcname * (varname list * exp)   // func f ( x1, ..., xn ) = e

exception ERROR

// ----------------------------------------
//              | TUP     of exp list             // ( e1, ..., en )
//              | FN      of varname list * exp   // fn ( x1, ..., xn ) => e
//              | APP     of exp * exp list       // e ( e1, ..., en )

type private pos = string * int * int (* (filename, line, char) *)

let private errat kind ((filename, line, char) : pos) msg =
  printf "%s in %s at %d, %d: %s\n\n" kind filename line char msg
  raise ERROR

let private err kind msg =
  printf "%s: %s\n\n" kind msg
  raise ERROR

module Lexer =
  type token =
    | TEOF
    | TINT  of int     // Integer     = [0-9]+ 
    | TNAME of string  // Name        = [a-zA-Z][a-zA-Z0-9]* 
    | TPUNC of char    // Punctuation = one of , ; ( ) { } [ ] 
    | TOPER of string  // Operator    = one of + - * = < > == != <= >= || && 

  (* val show       : token -> string

  type lexer

  val openFile   : string -> lexer
  val openString : string -> lexer
  val close      : lexer -> unit   (* checks that lexer is at EOF *)
  val read       : lexer -> pos * token
  val unread     : lexer * token -> unit
  *)

  let show = function
    | TEOF    -> "<end-of-file>"
    | TINT i  -> sprintf "%d" i
    | TNAME n -> n
    | TPUNC c -> sprintf "%c" c
    | TOPER s -> s

  type lexer = {
    name           : string
    mutable buffer : token option
    istream        : System.IO.TextReader
    mutable line   : int
    mutable char   : int
  }

  let openFile filename = {
    name    = filename;
    buffer  = None;
    istream = new System.IO.StreamReader(filename);
    line    = 1;
    char    = 0;
  }

  let openString s = {
    name    = "<string>";
    buffer  = None;
    istream = new System.IO.StringReader(s);
    line    = 1;
    char    = 0;
  }

  let private lerrat kind (l : lexer) msg =
      errat kind (l.name, l.line, l.char) msg

  let close (l : lexer) =
    match l.buffer with
      | Some _ -> lerrat "lexical error" l "input not exhaused"
      | _      -> match l.istream.Peek() with
                    | -1 -> l.istream.Close()
                    | _  -> lerrat "lexical error" l "input not exhaused"

  let private readch (l : lexer) =
    let c = l.istream.Read()
    if c < 0 then None else Some (char c)

  let private peekch (l : lexer) =
    let c = l.istream.Peek()
    if c < 0 then None else Some (char c)

  let private skipch (l : lexer) =
    let _ = l.istream.Read()
    ()

  // Automaton
    
  let private input (l : lexer) =
    match readch l with
      | None      -> None
      | Some '\n' -> l.line <- l.line + 1; Some '\n'
      | Some c    -> l.char <- l.char + 1; Some c

  let private makename cs =
    TNAME (System.String.Concat (List.map string (List.rev cs)))
  
  let rec private readname (l : lexer) cs : token =
    match peekch l with
      | None   -> makename cs
      | Some c -> if System.Char.IsLetterOrDigit c then
                    skipch l
                    readname l (c :: cs)
                  else
                    makename cs

  let private makenumber cs =
    TINT (int (System.String.Concat (List.map string (List.rev cs))))
        
  let rec private readnumber (l : lexer) cs : token =
    match peekch l with
      | None   -> makenumber cs
      | Some c -> if System.Char.IsDigit c then
                    skipch l
                    readnumber l (c :: cs)
                  else
                    makenumber cs

  let rec private readtoken (l : lexer) =
    let p1 = (l.name, l.line, l.char)
    match input l with
      | None     -> (p1, TEOF)
      | Some '(' -> (p1, TPUNC '(')
      | Some ')' -> (p1, TPUNC ')')
      | Some '{' -> (p1, TPUNC '{')
      | Some '}' -> (p1, TPUNC '}')
      | Some '[' -> (p1, TPUNC '[')
      | Some ']' -> (p1, TPUNC ']')
      | Some ';' -> (p1, TPUNC ';')
      | Some ',' -> (p1, TPUNC ',')
      | Some '=' -> match peekch l with
                      | Some '=' -> skipch l; (p1, TOPER "==")
                      | Some '>' -> skipch l; (p1, TOPER "=>")
                      | _        -> (p1, TOPER "=")
      | Some '+' -> (p1, TOPER "+")
      | Some '-' -> (p1, TOPER "-")
      | Some '*' -> (p1, TOPER "*")
      | Some '/' -> (p1, TOPER "/")
      | Some '<' -> match peekch l with
                      | Some '=' -> skipch l; (p1, TOPER "<=")
                      | _        -> (p1, TOPER "<")
      | Some '>' -> match peekch l with
                      | Some '=' -> skipch l; (p1, TOPER ">=")
                      | _        -> (p1, TOPER ">")
      | Some '|' -> match peekch l with
                      | Some '|' -> skipch l; (p1, TOPER "||")
                      | _        -> (p1, TOPER "|")
      | Some '&' -> match peekch l with
                      | Some '&' -> skipch l; (p1, TOPER "&&")
                      | _        -> (p1, TOPER "&")
      | Some '!' -> match peekch l with
                      | Some '=' -> skipch l; (p1, TOPER "!=")
                      | _        -> (p1, TOPER "!")
      | Some c   -> if System.Char.IsWhiteSpace c then
                      readtoken l
                    else if System.Char.IsLetter c then
                      (p1, readname l [c])
                    else if System.Char.IsDigit c then
                      (p1, readnumber l [c])
                    else
                      lerrat "lexical error" l (sprintf "unexpected character: %c" c)

  let read (l : lexer) =
    match l.buffer with
      | Some t -> let p1 = (l.name, l.line, l.char)
                  l.buffer <- None
                  (p1, t)
      | None   -> readtoken l
      
  let unread (l : lexer) = function
    | TEOF -> ()
    | t    -> match l.buffer with
                | None   -> l.buffer <- Some t
                | _      -> lerrat "fatal error" l "cannot unread twice"

module FixityParser =
  // type 'b stream = unit -> 'b option
  type assoc     = LEFT | RIGHT
  type 'a prefx  = int * ('a -> 'a)
  type 'a infx   = int * assoc * ('a * 'a -> 'a)
  // type 'a chunk  = pos * 'a option * 'a prefx option * 'a infx option

  let private prefer_left  (pr, a, _) (pr', a', _)
    = pr  > pr' || pr = pr' && a = LEFT  && a' = LEFT

  let private prefer_right (pr, a, _) (pr', a', _)
    = pr' > pr  || pr = pr' && a = RIGHT && a' = RIGHT

  // Parser stack: 
  type private 'a stack =
    | NIL
    | FRAME of 'a stack * 'a prefx list * 'a * 'a infx

  // reduce_stack : stack -> ast -> ast
  // This function is applied when there are no more tokens on the
  // input stream and no more prefix operators to reduce.  It
  // reduces all operators on the stack:
  let rec private reduce_stack st e =
    match st with
      | NIL -> e
      | FRAME (st, ps, e', (_, _, make)) ->
        // If ps = p :: ..., then prec(s) > prec(p), since otherwise we
        // would have reduced p(e').  We reduce s(e', e) first, and then
        // reduce all prefixes afterwards. 
        let rec loop st e' =
          match st with
            | []              -> e'
            | (_, make) :: ps -> loop ps (make e')
        reduce_stack st (loop ps (make (e', e)))

  // reduce_prefix : stack -> prefx list -> ast -> ast
  // This function is applied when there are no more tokens on the
  // input stream.  It reduces all currently missing prefix
  // operators and then reduces the stack.
  let rec private reduce_prefix st ps e =
    match ps with
      | []              -> reduce_stack st e
      | (_, make) :: ps -> reduce_prefix st ps (make e)

  // Main parsing function: 
  let rec parse jux next =
    // parse_prefix : stack -> prefx list -> token list -> ast
    let rec parse_prefix st ps next =
      match next() with
        | None -> err "parse error" "expected more input"
        | Some ((pos, _, _, _) as chunk) -> parse_prefix' st ps chunk next
    and parse_prefix' st ps ch next =
      match ch with
        | (pos, _, Some p, _)          -> parse_prefix st (p :: ps) next
        | (pos, _, None, Some i)       ->
          errat "parse error" pos "infix operator in prefix position"
        | (pos, Some atom, None, None) -> parse_infix st ps atom next
        | (pos, None, None, None)      ->
          errat "parse error" pos "not an operator"
    and parse_infix st ps e next =
      match next() with
        | None       -> reduce_prefix st ps e
        | Some chunk ->
          // If next token is an infix operator, then parse it as
          // such, even if it is also a prefix operator. 
          parse_infix' st ps e chunk next
    and parse_infix' st ps E ch next =
      match (st, ps, ch) with
        | (NIL, [], (_ : pos, _, _, Some i)) ->
          parse_prefix (FRAME (st, [], E, i)) [] next
        | (NIL, [], (_, _, _, None))  ->
          parse_prefix' (FRAME (st, [], E, jux)) [] ch next
        | (FRAME (st', ps', E', ((pr', a', make') as i')), [], (pos, _, _, Some ((pr, a, make) as i))) ->
          if prefer_left i' i then
            parse_infix' st' ps' (make' (E', E)) ch next
          else if prefer_right i' i then
            parse_prefix (FRAME (st, [], E, i)) [] next
          else
            errat "parse error" pos "precedence conflict"
        | (FRAME (st', ps', E', ((pr', a', make') as i')), [], (pos, _, _, None)) ->
          if prefer_left i' jux then
            parse_infix' st' ps' (make' (E', E)) ch next
          else if prefer_right i' jux then
            parse_prefix' (FRAME (st, [], E, jux)) [] ch next
          else
            errat "parse error" pos "precedence conflict"
        | (st, (pr', make') :: ps, (pos, _, _, Some ((pr, a, make) as i))) ->
          if pr' > pr then
            parse_infix' st ps (make' E) ch next
          else if pr > pr' then
            parse_prefix (FRAME (st, (pr', make') :: ps, E, i)) [] next
          else
            errat "parse error" pos "precedence conflict"
        | (st, (pr', make') :: ps, (pos, _, _, None)) ->
          let (pr, a, make) = jux
          if pr' > pr then
            parse_infix' st ps (make' E) ch next
          else if pr > pr' then
            parse_prefix' (FRAME (st, (pr', make') :: ps, E, jux)) [] ch next
          else
            errat "parse error" pos "precedence conflict"
    let parse_prefix_main st ps next =
      // Identical to parse_prefix, but adds position
      match next() with
        | None -> err "parse error" "expected more input"
        | Some ((pos, _, _, _) as chunk) -> parse_prefix' st ps chunk next
    parse_prefix_main NIL [] next

// The parser of programs

let private call ((p1, _), (_, _)) =
  errat "parse error" p1 "not a function call"
//  function
//    | ((p1, VAR f), (p2, TUP es))  -> (p1, CALL (f, es))
//    | ((p1, VAR f), (p2, e2))      -> (p1, CALL (f, [e2]))
//    | ((p1, e1),    (p2, TUP es))  -> (p1, APP (e1, es))
//    | ((p1, e1),    (p2, e2))      -> (p1, APP (e1, [e2]))
  
let private jux = (99, FixityParser.LEFT, call)

let private wrap1 c (p1, e1)             = (p1, c e1)
let private wrap2 c ((p1, e1), (p2, e2)) = (p1, c (e1, e2))

let private expect (l, t) =
  let (p', t') = Lexer.read(l)
  if t' = t then
    ()
  else
    errat "parse error" p' (sprintf "expected token %s, got %s" (Lexer.show t) (Lexer.show t'))

let rec makeStms = function
  | []            -> INT 0 // TUP []
  | [(_, e)]      -> e
  | (_, e) :: pes -> SEQ (e, makeStms pes)
  
let rec private chunkifyExp (l : Lexer.lexer) () =
  match Lexer.read l with
    | (p, Lexer.TEOF)        -> None
    | (p, Lexer.TPUNC '(')   ->
      match parseExps l with
        | [(_, e)] -> expect (l, Lexer.TPUNC ')')
                      Some (p, Some (p, e), None, None)
        | _        -> errat "parse error" p "unexpected tuple syntax"
//        | pes      -> expect (l, Lexer.TPUNC ')')
//                      Some (p, Some (p, TUP (List.map snd pes)), None, None)
    | (p, Lexer.TPUNC '{')   ->
      match parseStms l with
        | [(_, e)] -> expect (l, Lexer.TPUNC '}')
                      Some (p, Some (p, e), None, None)
        | pes      -> expect (l, Lexer.TPUNC '}')
                      Some (p, Some (p, makeStms pes), None, None)
    | (p, Lexer.TINT i)      -> Some (p, Some (p, INT i), None, None)
    | (p, Lexer.TOPER "+")   -> Some (p, None, None, Some (5, FixityParser.LEFT, wrap2 ADD))
    | (p, Lexer.TOPER "-")   -> Some (p, None, Some (10, wrap1 NEG), Some (5, FixityParser.LEFT, wrap2 SUB))

    | (p, Lexer.TOPER "*")   -> Some (p, None, None, Some (6, FixityParser.LEFT, wrap2 MUL))
    | (p, Lexer.TOPER "/")   -> Some (p, None, None, Some (6, FixityParser.LEFT, wrap2 DIV))
    | (p, Lexer.TOPER "==")  -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 EQ))
    | (p, Lexer.TOPER "!=")  -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 NEQ))
    | (p, Lexer.TOPER "<")   -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 LT))
    | (p, Lexer.TOPER ">")   -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 GT))
    | (p, Lexer.TOPER "<=")  -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 LE))
    | (p, Lexer.TOPER ">=")  -> Some (p, None, None, Some (4, FixityParser.LEFT, wrap2 GE))
    | (p, Lexer.TOPER "&&")  -> Some (p, None, None, Some (3, FixityParser.LEFT, wrap2 AND))
    | (p, Lexer.TOPER "||")  -> Some (p, None, None, Some (2, FixityParser.LEFT, wrap2 OR))

    (* Parsing let-expressions: *)
    | (p, Lexer.TNAME "let") ->
      match Lexer.read l with
        | (px, Lexer.TNAME x) ->
          expect (l, Lexer.TOPER "=")
          let (p1, e1) = parseExp l
          expect (l, Lexer.TNAME "in")
          let (p2, e2) = parseExp(l)
          Some (p, Some (p, LET (x, e1, e2)), None, None)
        | (p', _) -> errat "parse error" p' "expected a name"

    (* Parsing if-expressions: *)
    | (p, Lexer.TNAME "if") ->
      let (p1, e1) = parseExp l
      expect (l, Lexer.TNAME "then")
      let (p2, e2) = parseExp l
      expect (l, Lexer.TNAME "else")
      let (p3, e3) = parseExp l
      Some (p, Some (p, IF (e1, e2, e3)), None, None)

    (* Parsing exception-expressions: *)
    | (p, Lexer.TNAME "catch") -> Some (p, None, None, Some (3, FixityParser.LEFT, wrap2 CATCH))
    | (p, Lexer.TNAME "fail") -> Some (p, Some (p, FAIL), None, None)

    (* Parsing lambda-expressions: *)
#if LAMBDA
    | (p, Lexer.TNAME "fn") ->
      expect (l, Lexer.TPUNC '(')
      let xs = parseVars l
      expect (l, Lexer.TPUNC ')')
      expect (l, Lexer.TOPER "=>")
      let (p, e) = parseExp(l)
      Some (p, Some (p, FN (xs, e)), None, None)
#endif

    (* Follow-tokens for expressions *)
    | (p, (Lexer.TPUNC ')' as t))    -> Lexer.unread l t; None
    | (p, (Lexer.TPUNC '}' as t))    -> Lexer.unread l t; None
    | (p, (Lexer.TPUNC ';' as t))    -> Lexer.unread l t; None
    | (p, (Lexer.TPUNC ',' as t))    -> Lexer.unread l t; None
    | (p, (Lexer.TNAME "in" as t))   -> Lexer.unread l t; None
    | (p, (Lexer.TNAME "then" as t)) -> Lexer.unread l t; None
    | (p, (Lexer.TNAME "else" as t)) -> Lexer.unread l t; None 
    
    | (p, Lexer.TNAME n)           ->
      match Lexer.read l with
        | (p, Lexer.TPUNC '(') ->
          let pes = parseExps l
          expect (l, Lexer.TPUNC ')')
          Some (p, Some (p, CALL (n, List.map snd pes)), None, None)
        | (p, t) ->
          Lexer.unread l t
          Some (p, Some (p, VAR n), None, None)

    | (p, t) -> errat "parse error" p (sprintf "unexpected token: %s" (Lexer.show t))

and private parseExp l = FixityParser.parse jux (chunkifyExp l)

// parseExps and parseExpList parse tuples, after the initial '(',
// accordint to the grammar:
//     exp     ::= ( exps )
//     exps    ::= /* empty */
//               | explist
//     explist ::= exp
//               | exp , explist

and private parseExps l = // after '(' 
  match Lexer.read l with
    | (p, (Lexer.TPUNC ')' as t)) -> Lexer.unread l t; []
    | (p, t) -> Lexer.unread l t; parseExpList l

and private parseExpList l =
  let (p1, e1) = parseExp l
  match Lexer.read l with
    | (p, Lexer.TPUNC ',') -> let pes = parseExpList l
                              (p1, e1) :: pes 
    | (p, t) -> Lexer.unread l t; [(p1, e1)]

and private parseVars l = // after '(' 
  match Lexer.read l with
    | (p, (Lexer.TPUNC ')' as t)) -> Lexer.unread l t; []
    | (p, t) -> Lexer.unread l t; parseVarList l

and private parseVarList l =
  match Lexer.read l with
    | (p, Lexer.TNAME x) -> 
      match Lexer.read l with
      | (_, Lexer.TPUNC ',') -> let xs = parseVarList l
                                x :: xs 
      | (_, t) -> Lexer.unread l t; [x]
    | (p, t) -> errat "parse error" p (sprintf "unexpected token: %s" (Lexer.show t))

and private parseStms (l : Lexer.lexer) =
  match Lexer.read l with
    | (p, (Lexer.TPUNC '}' as t)) -> Lexer.unread l t; []
    | (p, t) ->
      Lexer.unread l t;
      let (p, e) = parseExp l
      match Lexer.read l with
        | (p, (Lexer.TPUNC ';' as t)) -> let pes = parseStms l
                                         (p, e) :: pes 
        | (p, t) -> Lexer.unread l t; [(p, e)]

let rec private parseFuncs (l : Lexer.lexer) =
  match Lexer.read l with
    | (p, Lexer.TNAME "func") ->
      match Lexer.read l with
        | (_, Lexer.TNAME f) ->
          expect (l, Lexer.TPUNC '(')
          let xs = parseVars l
          expect (l, Lexer.TPUNC ')')
          expect (l, Lexer.TOPER "=")
          let (p1, e1) = parseExp l
          expect (l, Lexer.TPUNC ';')
          (f, (xs, e1)) :: parseFuncs l
        | (p', _) -> errat "parse error" p' "expected function name"
    | (_, t) -> Lexer.unread l t; []

let private parseProg(l) =
  let fs = parseFuncs l
  let (p, e) = parseExp l
  (fs, e)

// Top-level parsing functions

let parseExpFromString s =
  let l = Lexer.openString s
  let (p, e) = parseExp l
  Lexer.close l
  e

let parseExpFromFile f =
  let l = Lexer.openFile f
  let (p, e) = parseExp l
  Lexer.close l
  e

let parseProgFromString s : func list * exp =
  let l = Lexer.openString s
  let (fs, e) = parseProg l
  Lexer.close l
  (fs, e)

let parseProgFromFile f : func list * exp =
  let l = Lexer.openString f
  let (fs, e) = parseProg l
  Lexer.close l
  (fs, e)
