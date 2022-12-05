module Solutions.Five

open System

module cargo =
    type ship = list<char> list
    type craneInstruction = { amt: int; from: int; to': int }
    type machine = CrateMover9000 | CrateMover9001

    let parseInstruction (ins: string) = 
        match ins.Split " " with
            | [| "move"; amt; "from"; from; "to"; to' |] -> { amt = int amt; from = int from; to' = int to' }
            | _ -> failwith "L + ratio + badfile"
 
    let parseShip: string seq -> ship =
        Seq.transpose
            >> Seq.map (Seq.filter Char.IsLetter)
            >> Seq.filter (fun s -> Seq.length s > 0)
            >> Seq.map (Seq.toList)
            >> Seq.toList

    let applyInstruction machine instruction (ship: ship): ship =
        let cargoBlock = ship.Item (instruction.from - 1) |> List.take instruction.amt
        let movedCargo = match machine with
                         | CrateMover9000 -> List.rev cargoBlock
                         | CrateMover9001 -> cargoBlock

        // so ugly :(((
        let newFrom = ship.Item (instruction.from - 1) |> List.skip instruction.amt
        let newTo =  movedCargo @ ship.Item (instruction.to' - 1)
        ship |> List.mapi (
            fun i x ->
                match i with
                | i when i = instruction.from - 1 -> newFrom
                | i when i = instruction.to' - 1 -> newTo
                | _ -> x)


let solveWith (machine: cargo.machine) (file: string seq) =
    let ship = file |> Seq.takeWhile (fun s -> s <> "") |> cargo.parseShip
    let instructions = file |> Seq.skipWhile (fun s -> s <> "") |> Seq.skip 1 |> Seq.map cargo.parseInstruction

    instructions 
        |> Seq.fold (fun s i -> cargo.applyInstruction machine i s) ship
        |> List.fold (fun p x -> p + string (List.head x)) ""

let partOne: string seq -> string = solveWith cargo.CrateMover9000
let partTwo: string seq -> string = solveWith cargo.CrateMover9001