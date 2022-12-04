open System
open System.IO
open Solutions

type solver = seq<string> -> int
type problemSolution = { partOne: solver; partTwo: solver }

let problemRunners = Map [
    (1, { partOne = One.onePartOne; partTwo = One.onePartTwo })
    (2, { partOne = Two.partOne; partTwo = Two.partTwo })
    (3, { partOne = Three.partOne; partTwo = Three.partTwo })
    (4, { partOne = Four.partOne; partTwo = Four.partTwo })
]

[<EntryPoint>]
let main args =
    let day = args[0] |> int
    let part = args[1] |> int

    match problemRunners.TryFind day with
    | Some solvers -> System.IO.File.ReadLines($"Inputs/{day}.txt") |> (if part = 1 then solvers.partOne else solvers.partTwo) |> printf "%d\n"
    | None -> printf $"no solver for day {day} part {part} exists\n"

    0