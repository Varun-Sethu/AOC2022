module Solutions

open System

let rec parseCalorieRecords = function
    | []         -> [[]]
    | "" :: xs   -> [] :: parseCalorieRecords xs
    | x :: xs    -> parseCalorieRecords xs |> fun (b :: bs) -> (b @ [int x]) :: bs

let totalCalorieRecords: seq<string> -> int list = List.ofSeq >> parseCalorieRecords >> List.map List.sum >> List.sortByDescending id

let onePartOne: seq<string> -> string = totalCalorieRecords >> List.head >> string
let onePartTwo: seq<string> -> string = totalCalorieRecords >> List.take 3 >> List.sum >> string