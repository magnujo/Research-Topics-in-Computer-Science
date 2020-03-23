//1

let rec product = function
  |[] -> 0 
  |x::xs -> x + product(xs)

//2
let rec count (x, ys) = 
  match ys with
    |[]      -> 0 
    |y :: ys -> if x = y then 1 + count(x, ys) else count(x, ys)

count(4, [])

//3

let rec sum(p, xs) = 
  match xs with
    |[]         -> 0 
    |x :: xs    -> if p x = true then 1 + sum(p, xs) else sum(p, xs)

sum(p, [2;1;3;1;1])
                      

//4

let rec split = function
  |[]      -> ([], [])
  |[y]     -> ([], [y])
  |y0 :: y1 :: ys -> let (l, r) = split ys 
                     (y0::l, y1::r)
               
split [1;10;2;3;4;5]




(* 
split 1::2::3::4::5[] ---> (l, r) = split 3::4::5[] = (3::[], 4::5[])
                          (y0::l, y1::r) = (1::3::[], 2::4::5::[]) 

split 3::4::5[]       ---> (l, r) = split 5::[] = ([], 5::[])
                           (y0::l, y1::r) = (3::l, 4::r) = (3::[], 4::5[])

split 5::[]             ---> ([], 5::[])

*)



let rec sumProd = function
  | [] -> (0,1)
  | x::xs ->
            let (rSum,rProd) = sumProd xs
            (x+rSum,x*rProd)


(* 
sumProd 1::2::[] ---> (rSum, rProd) = sumProd 2::[] = (2+0,2*1)
                      (1+rSum,1*rProd) = (1+2+0,1*2*1)

sumProd 2::[]    ---> (rSum, rProd) = sumProd [] = (0,1)
                      (2+rSum,2*rProd) = (2+0,2*1)

sumProd []       ---> (0,1) 
*)


//5a

let rec isWeaklyAscending(xs) =
  match xs with 
    |[] -> true
    |x::[] -> true
    |x0 :: x1 :: xs -> if x0 <= x1 then isWeaklyAscending(x1::xs)
                         else false 


isWeaklyAscending([1;2])



//5b
let rec count (x, ys) = 
  match ys with
    |[]      -> 0 
    |y0 :: y1 :: ys -> if  x = y0 then
                         if y1 > x then 1
                         else 1 + count(x, y1::ys)
                        else count(x, y1::ys)


count(2, [1;2;2;2;3;3;4])





//5c
                                                  

let rec intersect = function
  |(ys, []) -> []  
  |([], xs) -> []  
  |(x::xs, y::ys) -> if x = y then x::intersect(xs, y::ys)
                     else if x < y then intersect(xs, y::ys)  
                     else intersect (x::xs, ys)  

intersect ([1; 2; 3; 3; 5;], [1; 1; 3; 3; 3])

//5d
let rec union = function 
  |([], []) -> []
  |(xs, []) -> xs
  |([], ys) -> ys
  |(x::xs, y::ys) -> if x = y then x::y::union(xs,ys)
                     else if x < y then x::union(xs, y::ys)
                     else y::union(x::xs, ys)


union ([2; 2; 2; 3; 3; 5; 8], [1; 3; 3; 3; 4; 5; 10])


//[1; 2; 3; 3; 3; 3; 3; 4; 5; 5; 8]

//6

let rec split = function
  |[]      -> ([], [])
  |[y]     -> ([], [y])
  |y0 :: y1 :: ys -> let (l, r) = split ys 
                     (y0::l, y1::r)             
               
let rec union = function 
  |([], []) -> []
  |(xs, []) -> xs
  |([], ys) -> ys
  |(x::xs, y::ys) -> if x = y then x::y::union(xs,ys)
                     else if x < y then x::union(xs, y::ys)
                     else y::union(x::xs, ys)


let rec mergesort = function
  | []              -> []
  | [x]             -> [x]
  | x0 :: x1:: xs   -> let (l,r) = split xs
                       if x0 > x1 then mergesort l @ mergesort r
                       else mergesort l @ [x1] @ mergesort r
                       
                  


mergesort [2;1]

(* 
mergesort 1::10::2::3::5::5::[] --> (l,r) = split 1::10::2::3::5::5::[] = (1::2::5::[], 10::3::5::[]
                                       mergesort l @ mergesort r = 1::2::5::10::3::5::[]
split 1::10::2::3::5::5::[] --> (1::2::5::[], 10::3::5::[])
 *)

