module Solutions.Seven

module Interpetter =
    open Log
    type InterpretterState = FS.FSNode list

    let pushFSNode node (state: InterpretterState) = node :: state
    let getHead (state: InterpretterState) = List.head state
    let popHead (state: InterpretterState) = if List.length state = 0 then [] else List.tail state
    let emptyState: InterpretterState = [FS.Dir ("/", [])]

    let processLine state (logLine: LogLine) =
        let currentNode = getHead state
        let stateTail = popHead state
        match logLine with
            | File (name, size)   -> pushFSNode (FS.createEntity (FS.File (name, size)) currentNode) stateTail
            | Dir (newDir)        -> pushFSNode (FS.createEntity (FS.Dir (newDir, [])) currentNode) stateTail
            | Command (Ls)        -> state
            | Command (Cd "..")   -> let newHead = getHead stateTail
                                     let newStateTail = popHead stateTail
                                     in pushFSNode (FS.createEntity currentNode newHead) newStateTail
            | Command (Cd newDir) -> pushFSNode (FS.Dir (newDir, [])) state

    // evaluateState collapses the state stack until 1 element is left and returns that element
    let rec evaluateState stateStack =
        match List.length stateStack with
            | 1 -> List.head stateStack
            | _ -> processLine stateStack (Command (Cd "..")) |> evaluateState


let readFSState = Seq.skip 1 >> Log.parseLog >> Seq.fold Interpetter.processLine Interpetter.emptyState >> Interpetter.evaluateState

let partOne: string seq -> string = readFSState >> FS.getNodeStatistics >> List.filter (fun (_, dirSize) -> dirSize < 100000) >> List.sumBy (fun (_, dirSize) -> dirSize) >> string

let partTwo (file: string seq) =
    let rootDir = readFSState file
    let avaliableSpace = 70000000 - FS.getDirectorySize "/" rootDir
    let requiredSpace = 30000000 - avaliableSpace
    FS.getNodeStatistics rootDir
        |> List.filter (fun (_, size) -> size > requiredSpace) 
        |> List.minBy (fun (_, size) -> size) 
        |> string