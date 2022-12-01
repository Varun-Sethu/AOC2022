module Solutions

open System

let rec getCalorieRecords = function
    | []         -> [[]]
    | "" :: xs   -> [] :: getCalorieRecords xs
    | x :: xs    -> getCalorieRecords xs |> fun bs -> (List.head bs @ [int x]) :: List.tail bs

let getNetCaloriesRecord: seq<string> -> int list = List.ofSeq >> getCalorieRecords >> List.map List.sum >> List.sortByDescending id

let onePartOne: seq<string> -> string = getNetCaloriesRecord >> List.head >> string
let onePartTwo: seq<string> -> string = getNetCaloriesRecord >> List.take 3 >> List.sum >> string