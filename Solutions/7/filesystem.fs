module FS

[<CustomEquality>]
[<NoComparison>]
type FSNode =
    | File of string * int
    | Dir of string * list<FSNode>
    override this.Equals (other) =
        match other with
            | :? FSNode as other -> 
                match (this, other ) with
                    | (Dir (name1, _)), (Dir (name2, _))   -> name1 = name2
                    | (File (name1, _)), (File (name2, _)) -> name1 = name2
                    | _ -> false
            | _ -> false
            
let createEntity newEntity =
    function
        | Dir (name, files) -> 
            match List.tryFindIndex ((=) newEntity) files with
                | Some index -> Dir (name, List.mapi (fun i x -> if i = index then newEntity else x) files)
                | None -> Dir (name, newEntity :: files)
        | File _ -> failwith ":("

let rec private getNodeSize = function
    | File (_, size) -> size
    | Dir  (_, directoryFiles) -> directoryFiles |> List.map getNodeSize |> List.sum

let rec getNodeStatistics = function
    | File (_, _) -> []
    | Dir  (a, b) -> let currNodeStat = (a, getNodeSize (Dir (a, b)))
                     let remainingStats =  (b |> List.map getNodeStatistics |> List.concat)
                     in currNodeStat :: remainingStats 

let getDirectorySize dirname = 
    getNodeStatistics 
        >> List.find (fun (name, _) -> name = dirname) 
        >> snd