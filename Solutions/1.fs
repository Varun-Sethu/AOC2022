module Solutions.One

open System

let rec parseCalorieRecords = function
    | []         -> [[]]
    | "" :: xs   -> [] :: parseCalorieRecords xs
    | x :: xs    -> parseCalorieRecords xs |> fun (b :: bs) -> (b @ [int x]) :: bs

let totalCalorieRecords: seq<string> -> int list = List.ofSeq >> parseCalorieRecords >> List.map List.sum >> List.sortByDescending id

let onePartOne: seq<string> -> int = totalCalorieRecords >> List.head
let onePartTwo: seq<string> -> int = totalCalorieRecords >> List.take 3 >> List.sum