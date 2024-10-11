let fibIterative iters = 
    let mutable val1 = 0
    let mutable val2 = 1
    for i in 1..iters do
        let temp = val1 + val2
        val1 <- val2
        val2 <- temp
    val2

let rec fibRecurse x = 
    if x <= 1 then 
        1 
    else 
        (fibRecurse (x-1)) + (fibRecurse (x-2))


[<EntryPoint>]
let main argv =
    // let iter = argv[0]
    let iter = 4
    printfn $"Fibonacci (iterative): {iter} iterations returns {fibIterative iter}"
    printfn $"Fibonacci (recursive): {iter} iterations returns {fibRecurse iter}"
    0