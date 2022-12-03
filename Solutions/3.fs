module Solutions.Three

open System

let splitIntoComparments (sack: string) = seq { sack[0 .. sack.Length / 2 - 1]; sack[sack.Length / 2 ..] }
let priority = function
    | c when 'a' <= c && c <= 'z' -> int c - int 'a' + 1
    | c when 'A' <= c && c <= 'Z' -> int c - int 'A' + 27

let getDuplicateItem: string seq -> char =
    Seq.map (fun s -> s.ToCharArray() |> Set.ofArray) 
        >> Set.intersectMany 
        >> Set.toList 
        >> List.head

let partOne: seq<string> -> string = Seq.map (splitIntoComparments >> getDuplicateItem >> priority) >> Seq.sum >> string
let partTwo: seq<string> -> string = Seq.chunkBySize 3 >> Seq.map (getDuplicateItem >> priority) >> Seq.sum >> string