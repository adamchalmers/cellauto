module GameOfLife exposing (Cell, evolveCell, defaultCell, initBoard, onClick, cssClass, pickleCell, unpickleCell)

import List
import Set

-- How many neighbours must be alive for a (Live cell to remain Live, Dead cell to become Live respectively)
-- Survive 2/3 Born 3 defines the standard Conway Game of Life rules.
-- Changing these numbers produces different CA behaviours!
(survive, born) = (Set.fromList [2, 3], Set.fromList [3])


type Cell = Live | Dead

onClick : Cell -> Cell
-- What edit should be made to a cell when the user clicks it?
onClick c =
    case c of
        Live -> Dead
        Dead -> Live

defaultCell : Cell
-- When adding a new row/column to the CA grid, what value should they have?
defaultCell = Dead

cssClass : Cell -> String
cssClass cell =
    case cell of
        Live -> "live"
        Dead -> "dead"

initBoard : (Int, Int) -> Cell
-- Called to initialize the CA grid.
initBoard (i, j) =
    let
        startCells = [(10,10), (10,11), (10,12), (11,11), (20,10), (20,11), (20,12), (20,13), (9, 19), (10,20), (10,21), (10,22)]
            |> Set.fromList
    in
        if (Set.member (i,j) startCells) then Live else Dead

evolveCell : Cell -> List Cell -> Cell
-- Steps the cell forward. Implements the CA rules.
evolveCell cell neighs =
    let
        numLive = List.length <| List.filter ((==) Live) neighs
    in
        case cell of
            Live ->
                if Set.member numLive survive
                then Live
                else Dead
            Dead ->
                if Set.member numLive born
                then Live
                else Dead

pickleCell : Cell -> String
pickleCell cell =
    case cell of
        Live -> "L"
        Dead -> "D"

unpickleCell : String -> Cell
unpickleCell str =
    case str of
        "L" -> Live
        "D" -> Dead
        _ -> Dead
