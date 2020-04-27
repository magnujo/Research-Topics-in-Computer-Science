
(*

[IPUSH 5; ICALL 1; ILAB 2; ISWAP; IPOP; IHALT; ILAB 1; IGET 1; ICALL 0;
   ILAB 4; ISWAP; IPOP; ICALL 0; ILAB 3; ISWAP; IPOP; ISWAP; IRETN; ILAB 0;
   IGET 1; IPUSH 42; IADD; ISWAP; IRETN]


    IPUSH   5         5
    ICALL   L1      2 5
  L2: 
    ISWAP          5 89 
    IPOP           89
    IHALT          -> 89
  L1:    
    IGET    1      5 2 5 
    ICALL   L0     4 5 2 5 
  L4: 
    ISWAP          5 47 2 5
    IPOP           47 2 5
    ICALL   L0     3 47 2 5
  L3: 
    ISWAP          47 89 2 5
    IPOP           89 2 5 
    ISWAP          2 89 5
    IRETN          89 5
  L0:
    IGET    1     5 4 5 2 5          47 3 47 2 5
    IPUSH   42    42 5 4 5 2 5       42 47 3 47 2 5
    IADD          47 4 5 2 5         89 3 47 2 5
    ISWAP         4 47 5 2 5         3 89 47 2 5
    IRETN         47 5 2 5           89 47 2 5

  *)