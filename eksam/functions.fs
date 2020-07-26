
//Defining functions
let func x = 2 + x  
let func2 y = 3 + y

func 2

// Recursive functions
let rec sumupTo = function
    |0 -> 0
    |n -> n + sumupTo(n - 1)

sumupTo 2 

// Anonymous functions
//let fenv = List.map (fun (f, _) -> (f, newLabel())) funcs


// Function that take functions as input 
let ff (func, x) = func x + 2


// The type of a function
let u (x, y) = x + y  //uncurried
let c x y = x + y     //curried

let Curr x = c x

Curr 8 

u (3, 4)



