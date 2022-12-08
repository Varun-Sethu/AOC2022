module Solutions.Seven

module FS = 
    type FSNode =
        | File of string * int
        | Dir of string * list<FSNode>

    let private createOrUpdateEntity (comparer: FSNode -> FSNode -> bool) newEntity =
        function
            | Dir (name, files) -> match List.tryFindIndex (comparer newEntity) files with
                                    | Some index -> Dir (name, List.mapi (fun i x -> if i = index then newEntity else x) files)
                                    | None -> Dir (name, newEntity :: files)
            | File _ -> failwith ":("

    let createOrUpdateFile = createOrUpdateEntity (=)
    let createOrUpdateDir = createOrUpdateEntity (fun a b -> match (a, b) with | (Dir (name1, _)), (Dir (name2, _)) -> name1 = name2 | _ -> false)

    let rec private getTotalSize = function
        | File (_, size) -> size
        | Dir  (_, directoryFiles) -> directoryFiles |> List.map getTotalSize |> List.sum

    let rec getDirectoryStatistics =
        function
            | File (_, _) -> []
            | Dir  (a, b) -> (a, getTotalSize (Dir (a, b))) :: (b |> List.map getDirectoryStatistics |> List.concat) 

module Log = 
    type Command = Cd of string | Ls
    type LogLine = 
        | Command of Command
        | Dir of string 
        | File of string * int 

    let private parseLogLine (line: string) = 
        match line.Split ' ' with
            | [| "$"; "cd"; dir |] -> Command (Cd dir)
            | [| "$"; "ls" |]      -> Command Ls
            | [| "dir"; name |]    -> Dir name
            | [| fileSize; name |] -> File (name, int fileSize)
            | _ -> failwith "corrupted log"

    let parseLog: string seq -> LogLine seq = Seq.map parseLogLine

module Interpetter =
    open Log
    type InterpretterState = FS.FSNode list

    let processLine stateStack (logLine: Log.LogLine) =
        let currentNode = List.head stateStack
        let remainingNodes = List.skip 1 stateStack

        match logLine with
            | Command (Cd "..")   -> (FS.createOrUpdateDir currentNode (List.head remainingNodes)) :: (List.skip 1 remainingNodes)
            | Command (Cd newDir) -> (FS.Dir (newDir, [])) :: stateStack
            | Command (Ls)        -> stateStack
            | Dir (newDir)        -> (FS.createOrUpdateDir (FS.Dir (newDir, [])) currentNode) :: remainingNodes
            | File (name, size)   -> (FS.createOrUpdateFile (FS.File (name, size)) currentNode) :: remainingNodes

    // collapse state collapses a state stack until only the root element is left
    let rec collapseState stateStack =
        match List.length stateStack with
            | 1 -> stateStack
            | _ -> processLine stateStack (Command (Cd "..")) |> collapseState


let readFSState = Log.parseLog >> Seq.skip 1 >> Seq.fold Interpetter.processLine ([FS.Dir ("/", [])]) >> Interpetter.collapseState >> Seq.head

let partOne: string seq -> string = readFSState >> FS.getDirectoryStatistics >> List.filter (fun (_, dirSize) -> dirSize < 100000) >> List.sumBy (fun (_, dirSize) -> dirSize) >> string

let partTwo (file: string seq) =
    let fsState = file |> readFSState |> FS.getDirectoryStatistics
    let (_, usedSpace) = fsState |> List.find (fun (name, _) -> name = "/")
    fsState
        |> List.filter (fun (_, size) -> size > (30000000 - (70000000 - usedSpace))) 
        |> List.minBy (fun (_, size) -> size) |> string