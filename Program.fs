open System
open System.IO
open Solutions

let problemRunners = Map [
    (1, Solutions.one)
]

[<EntryPoint>]
let main args =
    let day = args[0] |> int

    match problemRunners.TryFind day with
    | Some solver -> System.IO.File.ReadLines($"Inputs/{day}.txt") |> solver |> printf "%s"
    | None -> printf $"no solver for day {day} exists\n"

    0