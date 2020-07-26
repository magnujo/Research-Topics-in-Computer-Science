  
type weekday = | MONDAY    | TUESDAY   | WEDNESDAY   | THURSDAY
               | FRIDAY    | SATURDAY  | SUNDAY

let isWeekend = function
                  | SATURDAY -> true
                  | SUNDAY   -> true
                  | _        -> false

type intOption = | NONE 
                 | SOME of int

let workload = function 
                 | SATURDAY -> NONE
                 | SUNDAY   -> NONE
                 | FRIDAY   -> SOME 6
                 | _        -> SOME 8