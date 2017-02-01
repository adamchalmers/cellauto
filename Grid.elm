module Grid exposing (Grid, Index, init, initAs, map, indexedMap, fromLists, toLists, get, set, transpose, length, height, neighbours, mutate, incSize, decSize)

import Array as A
import List
import Maybe exposing (andThen)

-- EXPOSED FUNCTIONS

type alias Grid a = A.Array (A.Array a)
type alias Index = (Int, Int)

length : Grid a -> Int
length = A.length

height : Grid a -> Int
height g =
    case A.get 0 g of
        Nothing -> Debug.crash "Grid doesn't have a second dimension."
        Just array -> A.length array

init : a -> Int -> Int -> Grid a
init val w h = (A.initialize w (always (A.initialize h (always val))))

initAs : (Index -> a) -> Int -> Int -> Grid a
initAs f w h = (A.initialize w (\i -> A.initialize h (\j -> f (i, j))))

map : (a -> b) -> Grid a -> Grid b
map f grid =
    A.map (A.map f) grid

indexedMap : (Index -> a -> b) -> Grid a -> Grid b
indexedMap f grid =
    let
        rowFn : Int -> (A.Array a) -> A.Array b
        rowFn i row = A.indexedMap (\j elem -> f (i, j) elem) row
    in
        A.indexedMap rowFn grid

fromLists : List (List a) -> Grid a
fromLists ls = A.fromList <| List.map A.fromList ls

toLists : Grid a -> List (List a)
toLists g = A.toList <| A.map (A.toList) g

get : Index -> Grid a -> Maybe a
get (i, j) grid = (A.get j grid) |> andThen (A.get i)

set : Index -> a -> Grid a -> Grid a
set (j, i) val grid =
    let
        row = A.get i grid
    in
        case row of
            Just r -> (A.set i (A.set j val r) grid)
            Nothing -> grid

mutate : Index -> (a -> a) -> Grid a -> Grid a
-- mutate applies function (f) to the value at (index) if it exists.
mutate index f g =
    let
        curr = get index g
    in
        case curr of
            Nothing -> g
            Just val -> set index (f val) g

transpose : Grid a -> Maybe (Grid a)
transpose grid =
    let
        r = List.range 0 (-1 + length grid)
    in
        Maybe.map A.fromList (unmaybeList <| List.map (\i -> row i grid) r)


neighbours : Index -> Grid a -> List a
neighbours (x, y) g =
    let
        cands =
            [ get (x, y) g
            , get (x + 1, y) g
            , get (x - 1, y) g
            , get (x, y + 1) g
            , get (x, y - 1) g
            , get (x + 1, y + 1) g
            , get (x + 1, y - 1) g
            , get (x - 1, y + 1) g
            , get (x - 1, y - 1) g
            ]
    in
        List.filterMap identity cands

incSize : a -> Grid a -> Grid a
-- Increments the size of the grid, adding a new row/col of the given value.
incSize val g =
    let
        grownRows = A.map (\row -> A.push val row) g
        newRow = A.initialize (1 + A.length g) (always val)
    in
        A.push newRow grownRows

decSize : Grid a -> Grid a
-- Decrements the grid by one row and one column.
decSize g =
    let
        shrunkRows = A.map (\arr -> A.slice 0 (-1 + A.length arr) arr) g
    in
        A.slice 0 (-1 + A.length g) shrunkRows

-- UNEXPOSED FUNCTIONS

row : Int -> Grid a -> Maybe (A.Array a)
row i grid = unmaybeArray <| A.map (A.get i) grid

unmaybeList : List (Maybe a) -> Maybe (List a)
unmaybeList elems =
    case elems of
        [] ->
            Just []
        (e::es) ->
            case (e, unmaybeList es) of
                (_, Nothing) -> Nothing
                (Nothing, _) -> Nothing
                (Just x, Just xs) -> Just (x::xs)

unmaybeArray : A.Array (Maybe a) -> Maybe (A.Array a)
unmaybeArray arr =
    Maybe.map A.fromList (unmaybeList <| (A.toList arr))

flatten : Grid a -> List a
flatten g = toLists g |> List.concat