module Solutions1

// 1(a)
let det ((a, c), (b, d)) = a * d - b * c

// 1(b)
let transpose ((a, c), (b, d)) = ((a, b), (c, d))

// 1(c)
let add ((a1, c1), (b1, d1)) ((a2, c2), (b2, d2)) =
  ((a1 + a2, b1 + b2), (c1 + c2, d1 + d2))

// 1(c)
let mul ((a1, c1), (b1, d1)) ((a2, c2), (b2, d2)) =
  ((a1 * a2 + b1 * c2, c1 * a2 + d1 * c2),
   (a1 * b2 + b1 * d2, c1 * b2 + d1 * d2))

// 2
let rec fac n =
  if   n = 0
  then 1
  else n * fac (n - 1)

// 3
let rec sumUpTo n =
  if n = 0
  then 0
  else n + sumUpTo (n - 1)
  
// 4
let solve a b c =
  let d = b * b - 4.0 * a * c
  ((-b - sqrt d) / (2.0 * a), (-b + sqrt d) / (2.0 * a))

// 5(a)
let tick (h, m, s) =
  if s = 59 then
    if m = 59 then
      (h + 1, 0, 0)
    else
      (h, m + 1, 0)
  else
    (h, m, s + 1)

// 5(b)
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

    
