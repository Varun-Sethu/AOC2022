module Solutions.Four

module range =
    type range = { rangeStart: int; rangeEnd: int }

    let parse (range: string) = match range.Split "-" with 
                                | [| start; end' |] -> { rangeStart = int start; rangeEnd = int end' }
                                | _ -> failwith "L + ratio + bad file"

    let parsePair (pair: string) = 
        pair.Split "," 
            |> function 
                | [| a; b |] -> (parse a, parse b)
                |  _ -> failwith "L + ratio + bad file"

    let areEnglufing ({ rangeStart = sa; rangeEnd = ea }, { rangeStart = bs; rangeEnd = eb }) = (sa >= bs && ea <= eb) || (sa <= bs && ea >= eb)
    let areOverlapping (a: range, b: range) = max a.rangeStart b.rangeStart <= min a.rangeEnd b.rangeEnd

let getNumPairsWhere predicate pairs =
    pairs
        |> Seq.filter (range.parsePair >> predicate)
        |> Seq.length

let partOne: string seq -> int = getNumPairsWhere range.areEnglufing
let partTwo: string seq -> int = getNumPairsWhere range.areOverlapping