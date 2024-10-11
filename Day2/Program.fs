/// Day 2: All Your Base
/// https://exercism.org/tracks/fsharp/exercises/all-your-base

let rec fromBase b num =
    if (num%10) > b then -1 // Bad input
    elif num = 0 then 0 // BC
    else (num % 10) + (b * (fromBase b (num / 10))) // IC
let rec toBase b num =
    if b = 0 then -1 // Bad input
    elif num = 0 then 0 // BC
    else (num%b) + (10 * (toBase b (num/b))) // IC
let convert oldBase newBase num = 
    toBase newBase (fromBase oldBase num)

let rec convertAll (fromList: list<int>) (toList: list<int>) (numList: list<int>): list<int> = 
    if not (fromList.Length = toList.Length && toList.Length = numList.Length) then [-1] // Bad input
    elif numList.IsEmpty || fromList.IsEmpty || toList.IsEmpty then [] // BC
    else [convert fromList.Head toList.Head numList.Head] // IC
        @ (convertAll fromList.Tail toList.Tail numList.Tail)

[<EntryPoint>]
let main argv =
    let fromList = [2; 3; 10]
    let toList = [10; 10; 3]
    let numList = [00101; 00101; 10] // outputs 5; 10; 101 
    let result = convertAll fromList toList numList
    printfn $"{result}"
    0