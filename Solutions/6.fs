module Solutions.Six

let numDistinct elements = (Set.count (Set.ofList elements))
let tryTake n x = if List.length x >= n then Some (List.take n x) else None  

let getSignalComponent componentSize =
    let distinct = numDistinct >> ((=) componentSize)
    let rec inner x signal = 
        match tryTake componentSize signal with
        | Some elements -> if distinct elements then x + componentSize else inner (x + 1) (List.skip 1 signal)
        | None -> -1
    in inner 0

let getMarkerLocation = getSignalComponent 4
let getMessageLocation = getSignalComponent 14

let partOne: string seq -> string = Seq.head >> Seq.toList >> getMarkerLocation >> string
let partTwo: string seq -> string = Seq.head >> Seq.toList >> getMessageLocation >> string