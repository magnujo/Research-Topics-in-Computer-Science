//3 



let rec maxSearch max =  function
        | []      -> max
        | x :: xs -> if x > max then maxSearch x xs else maxSearch max xs
                      
let max = 0
let rec maxSearch ys =
    match ys with
       | []      -> max
       | x :: xs -> if x > max then maxSearch xs else maxSearch xs
                    





