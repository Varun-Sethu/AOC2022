module Solutions.Four

module range =
    type range = { start: int; end': int }

    let parsePair (pair: string) = 
        pair.Split([|','; '-'|])
            |> function 
                | [| aStart; aEnd; bStart; bEnd |] -> ( { start = int aStart; end' = int aEnd }, { start = int bStart; end' = int bEnd })
                |  _ -> failwith "L + ratio + bad file"

    let areEnglufing ({ start = sa; end' = ea }, { start = bs; end' = eb }) = (sa >= bs && ea <= eb) || (sa <= bs && ea >= eb)
    let areOverlapping (a: range, b: range) = max a.start b.start <= min a.end' b.end'

let getNumPairsWhere predicate pairs =
    pairs
        |> Seq.filter (range.parsePair >> predicate)
        |> Seq.length

let partOne: string seq -> string = getNumPairsWhere range.areEnglufing >> string
let partTwo: string seq -> string = getNumPairsWhere range.areOverlapping >> string