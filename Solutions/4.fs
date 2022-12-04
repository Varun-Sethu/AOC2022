module Solutions.Four

type range = { rangeStart: int; rangeEnd: int }

let parseRange (range: string) = let ranges = range.Split "-" in { rangeStart = int ranges[0]; rangeEnd = int ranges[1] }
let parseRangePair (pair: string) = 
    pair.Split "," 
        |> fun [| a; b |] -> (parseRange a, parseRange b)

let contains range x = range.rangeStart <= x && x <= range.rangeEnd
let rangesEngulf (a: range, b: range) = 
    (contains a b.rangeStart && contains a b.rangeEnd) || 
    (contains b a.rangeStart && contains b a.rangeEnd)

let rangesOverlap (a: range, b: range) = max a.rangeStart b.rangeStart <= min a.rangeEnd b.rangeEnd

let partOne: string seq -> string = Seq.filter (parseRangePair >> rangesEngulf) >> Seq.length >> string
let partTwo: string seq -> string = Seq.filter (parseRangePair >> rangesOverlap) >> Seq.length >> string