module Solutions.Seven

module Interpetter =
    open Log
    type InterpretterState = FS.FSNode list

    let processLine stateStack (logLine: LogLine) =
        let currentNode = List.head stateStack
        let remainingNodes = List.skip 1 stateStack

        match logLine with
            | Command (Cd "..")   -> (FS.createOrUpdateEntity currentNode (List.head remainingNodes)) :: (List.skip 1 remainingNodes)
            | Command (Cd newDir) -> (FS.Dir (newDir, [])) :: stateStack
            | Command (Ls)        -> stateStack
            | Dir (newDir)        -> (FS.createOrUpdateEntity (FS.Dir (newDir, [])) currentNode) :: remainingNodes
            | File (name, size)   -> (FS.createOrUpdateEntity (FS.File (name, size)) currentNode) :: remainingNodes

    // collapse state collapses a state stack until only the root element is left
    let rec collapseState stateStack =
        match List.length stateStack with
            | 1 -> stateStack
            | _ -> processLine stateStack (Command (Cd "..")) |> collapseState


let readFSState = Log.parseLog >> Seq.skip 1 >> Seq.fold Interpetter.processLine ([FS.Dir ("/", [])]) >> Interpetter.collapseState >> Seq.head

let partOne: string seq -> string = readFSState >> FS.getNodeStatistics >> List.filter (fun (_, dirSize) -> dirSize < 100000) >> List.sumBy (fun (_, dirSize) -> dirSize) >> string

let partTwo (file: string seq) =
    let fsState = file |> readFSState |> FS.getNodeStatistics
    let (_, usedSpace) = fsState |> List.find (fun (name, _) -> name = "/")
    fsState
        |> List.filter (fun (_, size) -> size > (30000000 - (70000000 - usedSpace))) 
        |> List.minBy (fun (_, size) -> size) |> string