module GameOfLife exposing (Cell, evolveCell, defaultCell, initBoard, onClick, cssClass)

import List as L
import Set

type Cell = Live | Dead

onClick : Cell -> Cell
onClick c =
    case c of
        Live -> Dead
        Dead -> Live

defaultCell : Cell
defaultCell = Dead

cssClass : Cell -> String
cssClass cell =
    if cell == Live
    then "live"
    else "dead"

initBoard : (Int, Int) -> Cell
initBoard (i, j) =
    let
        startCells = [(10,10), (10,11), (10,12), (11,11), (20,10), (20,11), (20,12), (20,13), (9, 19), (10,20), (10,21), (10,22)]
            |> Set.fromList
    in
        if (Set.member (i,j) startCells) then Live else Dead

evolveCell : Cell -> List Cell -> Cell
evolveCell cell neighs =
    let
        numLive = count Live
        numDead = count Dead
        count val = L.length <| L.filter ((==) val) neighs
    in
        case cell of
            Live ->
                if numLive < 2 then
                    Dead
                else if (numLive == 2) || (numLive == 3) then
                    Live
                else
                    Dead
            Dead ->
                if numLive == 3 then
                    Live
                else
                    Dead
