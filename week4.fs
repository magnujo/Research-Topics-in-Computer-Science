(*   
1

(let a = 11 + 12 in 13 + a) + 14 = 50

  IPUSH   11  11
  IPUSH   12  12, 11
  IADD        23
  IPUSH   13  13, 23
  IGET     1  23,13,23
  IADD        36,23
  ISWAP       23,36
  IPOP        36
  IPUSH   14  14, 36
  IADD        50  
  
  *)


  //2

  (* 
  2 + (3 + 4)
   ADD (INT 2, ADD (INT 3, INT 4)) 

   IPUSH 2  2
   IPUSH 3  3, 2
   IPUSH 4  4, 3, 2
   IADD     7,2
   IADD     9


(2 + 3) + 4

IPUSH 2     2
IPUSH 3     3, 2
IADD        5
IPUSH 4     4, 5
IADD        9

   *)

//5a

type 'a tree = | LEAF
               | NODE of 'a * 'a tree * 'a tree 

let treemap f tree = 