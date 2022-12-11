module Solutions.Nine

type Rope = (int * int) list
type State = Rope * ((int * int) Set)
type Direction = Up | Down | Left | Right
    with
        static member emptyState (ropeSize: int): State = (List.init ropeSize (fun _ -> (0, 0)), Set.singleton (0, 0))
        member this.GetDelta ()
            = match this with
              | Up    -> (0, +1)
              | Down  -> (0, -1)
              | Left  -> (-1, 0)
              | Right -> (+1, 0)

// getElementDeltas computes the deltas in wich the next element moves given the previous elements position
let normalise x = if x = 0 then 0 else (if x <= -1 then -1 else 1)
let getElementDeltas (prevX, prevY) (currX, currY) =
    let deltaX = prevX - currX
    let deltaY = prevY - currY

    let isAdjacent = abs deltaY <= 1 && abs deltaX <= 1
    if isAdjacent then (0, 0) else (normalise deltaX, normalise deltaY)

let rec moveRopeBody prevElemPosition body = 
    match body with
        | [] -> []
        | (elmX, elmY) :: remElems -> 
            let (dX, dY) = getElementDeltas prevElemPosition (elmX, elmY)
            let (newX, newY) = (elmX + dX, elmY + dY)

            (newX, newY) :: (moveRopeBody (newX, newY) remElems)

// we assume that the provided directions are always valid
let moveRope (moveDir: Direction) (((headX, headY) :: rem, visited): State): State = 
    let (hDeltaX, hDeltaY) = moveDir.GetDelta()
    let newHead = (headX + hDeltaX, headY + hDeltaY)
    let movedRope = newHead :: moveRopeBody newHead rem

    (movedRope, visited.Add (List.last movedRope))

let repeat n fn = List.init n (fun _ -> fn) |> List.reduce (>>)
let parseCommand (x: string) = 
    match x.Split ' ' with
        | [| "R"; amt |] -> moveRope Right |> repeat (int amt)
        | [| "L"; amt |] -> moveRope Left |> repeat (int amt)
        | [| "U"; amt |] -> moveRope Up |> repeat (int amt)
        | [| "D"; amt |] -> moveRope Down |> repeat (int amt)

let solve (ropeSize: int) = Seq.map (parseCommand) >> Seq.fold (fun acc f -> f acc) (Direction.emptyState ropeSize) >> (fun (_, positions) -> positions.Count) >> string 

let partOne: string seq -> string = solve 2
let partTwo: string seq -> string = solve 10