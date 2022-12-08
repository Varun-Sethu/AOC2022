module Solutions.Eight

let parseGrid: string seq -> int array array = Seq.map (Seq.toArray >> Array.map (fun c -> int c - int '0')) >> Seq.toArray
let isOnBorder (numRows, numCols) (row, col) = row = 0 || col = 0 || row = numRows - 1 || col = numCols - 1

let existsTreeGreaterThan x = Seq.tryFindIndex (fun v -> v >= x) >> Option.isSome
let numVisibleTrees x seq = match Seq.tryFindIndex (fun v -> v >= x) seq with
                                | Some i -> i + 1
                                | None   -> Seq.length seq

let numTreesWhere pred = List.filter pred >> List.length
let potentialBlockingTrees (grid: int array array) numRows numCols (row, col) =
    seq {
        seq { for col' = col - 1 downto 0 do yield grid[row][col'] }; // left
        seq { for col' in col + 1 .. numCols - 1 do yield grid[row][col'] }; // right
        seq { for row' = row - 1 downto 0 do yield grid[row'][col] } // above
        seq { for row' in row + 1 .. numRows - 1 do yield grid[row'][col] } // below
    }


let isVisibleIn (grid: int array array) (numRows, numCols) (row, col) = 
    if isOnBorder (numRows, numCols) (row, col) then true
    else
        potentialBlockingTrees grid numRows numCols (row, col)
            |> Seq.exists (fun trees -> not (existsTreeGreaterThan (grid[row][col]) trees))

let scenicScoreFor (grid: int array array) (numRows, numCols) (row, col) = 
    if isOnBorder (numRows, numCols) (row, col) then 0
    else
        potentialBlockingTrees grid numRows numCols (row, col)
            |> Seq.map (numVisibleTrees (grid[row][col]))
            |> Seq.fold (*) 1


let partOne unparsedGrid = 
    let grid = parseGrid unparsedGrid
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    let isVisible = isVisibleIn grid (numRows, numCols)
    
    List.allPairs [0 .. numRows - 1] [0 .. numCols - 1] |> numTreesWhere (fun (row, col) -> isVisible (row, col)) |> string

let partTwo unparsedGrid =
    let grid = parseGrid unparsedGrid
    let (numRows, numCols) = (grid.Length, grid[0].Length)
    let getScenicScore = scenicScoreFor grid (numRows, numCols)

    List.allPairs [0 .. numRows - 1] [0 .. numCols - 1] |> List.map getScenicScore |> List.max |> string