
//Defining functions
let func x = 2 + x  
let func2 y = 3 + y

// Recursive functions
let rec sumupTo = function
    |0 -> 0
    |n -> n + sumupTo(n - 1)

// Anonymous functions
//let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs


// Function that take functions as input or produce functions as output
let ff (func, x) = func x + 2




// The type of a function
let c (x, y) = x + y  //curried
let u x y = x + y   //uncurried

let unCurr x = g x

unCurr 8

c (3, 4)



