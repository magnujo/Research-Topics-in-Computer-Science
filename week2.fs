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


let p x = x>0

sum(p, [2;1;3;1;1])
                      

//4

let rec split = function
  |[]      -> ([], [])
  |[y]     ->  y
  |y0 :: y1 :: ys -> let (l, r) = split ys 
                     (y0::l, y1::r)
               
split [1;2;3;4]




(* 
split 1::2::3::4[] ---> (l, r) = split 3::4::[] = (3::[], 4::[])
                          (y0::l, y1::r) = (1::l, 2::r) = (1::3::[], 2::4::[])

split 3::4::[]       ---> (l, r) = split [] = ([], [])
                          (y0::l, y1::r) = (3::l, 4::r) = (3::[], 4::[])

split []             ---> ([], [])

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


let rec count(x, ys) = 
  match ys with
    |