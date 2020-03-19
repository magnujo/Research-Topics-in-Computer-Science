let rec fact = function
    |0 -> 1
    |n -> n * fact(n - 1)

fact 4


let rec sumupTo = function
    |0 -> 0
    |n -> n + sumupTo(n - 1)

sumupTo 10

//5
let solve a b c = 
  let d = b * b - 4.0 * a * c
  ((-b - sqrt d) / (2.0 * a), (-b + sqrt d) / (2.0 * a))

//6a
let tick (h, m, s) = function
    |(h, 59, 59) -> (h+1, 0, 0)
    |(h, m, 59) -> (h, m, 0)
    |(h, m, s) -> (h, m, s+1)

tick (2, 59, 59)


//6a 
let tick (h, m, s) =
    if s = 59 then
        if m = 59 then
            (h+1, 0, 0)
        else 
            (h, m+1, 0)
    else
    (h, m, s+1)

tick (1, 0, 59)

//6b
let before (h1, m1, s1) (h2, m2, s2) =
  if h1 < h2 then 
    true
  else if h1 > h2 then
    false
  else if m1 < m2 then
    true
  else if m1 > m2 then
    false
  else if s1 < s2 then
    true
  else
    false

before (10, 21, 31) (10, 21, 30)


