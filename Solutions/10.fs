module Solutions.Ten

// delta @ application time
type Instruction = 
    | Add of int
    | Noop
    with
        static member parse (ins: string) =
            match ins.Split ' ' with
                | [| "addx"; amt |] -> Add (int amt)
                | [| "noop" |] -> Noop

        static member parseFile = Seq.map Instruction.parse >> Seq.toList

let getNumCycles instruction =
    match instruction with
        | Add _ -> 2
        | Noop  -> 1

// bit the bullet, imperative F# :(
let getCycleValues (queries: int list) (instructions: Instruction list) =
    let mutable answers = Map.empty<int, int>
    
    let mutable currValue = 1
    let mutable currCycle = 1
    let mutable currInstruction = 0

    for query in queries do
        // kinda hacky because F# doesnt have breaks but this is equivalent anyways
        if currInstruction <= instructions.Length then
            while currCycle + getNumCycles instructions[currInstruction] <= query do
                currCycle <- currCycle + getNumCycles instructions[currInstruction]
                match instructions[currInstruction] with
                    | Noop      -> ()
                    | Add delta -> currValue <- currValue + delta

                currInstruction <- currInstruction + 1
        else
            currCycle <- query
        // query can now be answered altho we must use the old value :D
        answers <- (Map.add query currValue answers)
    answers

let partOne: string seq -> string = Instruction.parseFile >> getCycleValues ([20; 60; 100; 140; 180; 220]) >> Map.fold (fun acc k v -> acc + k * v) 0 >> string
let partTwo: string seq -> string =
    Instruction.parseFile
        >> getCycleValues (List.init 240 (fun i -> i + 1))
        >> Map.map (fun k v -> (if abs ((k % 40) - 1 - v) <= 1 then "#" else "."))
        >> Map.fold (fun acc k c -> if k % 40 = 1 then acc + "\n" + c else acc + c) ""