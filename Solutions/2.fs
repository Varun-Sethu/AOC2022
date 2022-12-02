module Solutions.Two

open System

type hand = Rock = 0 | Paper = 1 | Scisors = 2
type round = { mine: hand; theirs: hand }
    
let parsePlay = function | "A" | "X" -> hand.Rock | "B" | "Y" -> hand.Paper | "C" | "Z" -> hand.Scisors
let parseRoundNaive (round: string[]) = { mine = parsePlay round[1]; theirs = parsePlay round[0] }

let parseRoundGigaBrain (round: string[]): round =
    let theirs = parsePlay round[0]
    let mine = match round[1] with
                | "X" -> ((int theirs - 1) + 3) % 3 |> enum
                | "Y" -> theirs
                | "Z" -> (int theirs + 1) % 3 |> enum

    { mine = mine; theirs = theirs }

let scoreHand hand = int hand + 1  
let scoreRound round = scoreHand round.mine + match round with
                                              | { mine = x; theirs = y } when int x = (int y + 1) % 3 -> 6
                                              | { mine = x; theirs = y } when x = y -> 3
                                              | _ -> 0 

let solve parser : seq<string> -> string = Seq.map (fun round -> round.Split ' ' |> parser |> scoreRound) >> Seq.sum >> string

let partOne: seq<string> -> string = solve parseRoundNaive
let partTwo: seq<string> -> string = solve parseRoundGigaBrain