module Solutions.Eight


let leftOf (row: int, col: int)                  = seq { for col' = col - 1 downto 0 do yield (row, col') }
let topOf (row: int, col: int)                   = seq { for row' = row - 1 downto 0 do yield (row', col) } 
let rightOf (row: int, col: int) (numCols: int)  = seq { for col' in col + 1 .. numCols - 1 do yield (row, col') }
let bottomOf (row: int, col: int) (numRows: int) = seq { for row' in row + 1 .. numRows - 1 do yield (row', col) }

let parseGrid: string seq -> int array array = Seq.map (Seq.toArray >> Array.map (fun c -> int c - int '0')) >> Seq.toArray
let indexGreaterThan x = Seq.tryFindIndex (fun v -> v >= x)

let existsIndexGreaterThan x = indexGreaterThan x >> Option.isSome
let numVisibleTrees x seq = match indexGreaterThan x seq with
                                | Some i -> i + 1
                                | None   -> Seq.length seq

let visibilityDirections numRows numCols (row, col) =
    seq { rightOf (row, col) numCols; leftOf (row, col); bottomOf (row, col) numRows; topOf (row, col) }

let isVisibleIn (grid: int array array) (row, col) = 
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    let isBorderNode = row = 0 || col = 0 || row = numRows - 1 || col = numCols - 1

    isBorderNode || (
        let isVisible = visibilityDirections numRows numCols (row, col)
                            |> Seq.map (Seq.map (fun (row', col') -> grid[row'][col']))
                            |> Seq.exists (existsIndexGreaterThan (grid[row][col]) >> not)
        isVisible
    )

let scenicScoreFor (grid: int array array) (row, col) = 
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    let isBorderNode = row = 0 || col = 0 || row = numRows - 1 || col = numCols - 1

    if isBorderNode then 0
    else
        visibilityDirections numRows numCols (row, col)
            |> Seq.map (Seq.map (fun (row', col') -> grid[row'][col']))
            |> Seq.map (numVisibleTrees (grid[row][col]))
            |> Seq.fold (*) 1


let partOne (unparsedGrid: string seq) = 
    let grid = parseGrid unparsedGrid
    let isVisible = isVisibleIn grid
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    
    List.allPairs [0 .. numRows - 1] [0 .. numCols - 1]
        |> List.filter (fun (row, col) -> isVisible (row, col))
        |> List.length
        |> string

let partTwo (unparsedGrid: string seq) =
    let grid = parseGrid unparsedGrid
    let getScenicScore = scenicScoreFor grid
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    
    List.allPairs [0 .. numRows - 1] [0 .. numCols - 1]
        |> List.map getScenicScore
        |> List.max
        |> string