module VM

// Instructions

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
