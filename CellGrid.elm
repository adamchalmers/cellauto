module CellGrid exposing (Model, Msg, update, view, init, initBoard, makeTick, evolveModel)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Grid as G exposing (Grid)
import List as L
import Time
import Debug

-- MODEL

type Cell = Live | Dead

invert : Cell -> Cell
invert c =
    case c of
        Live -> Dead
        Dead -> Live

type State = Stopped | Playing

type alias Model =
    { grid: G.Grid Cell
    , state: State
    }

globals = {width = "20px"}

init : (Model, Cmd msg )
init =
    ( initBoard 8
    , Cmd.none
    )

initBoard : Int -> Model
initBoard w =
    { grid = G.initAs (\(i,j) -> if (i%2==0) then Live else Dead) w w
    , state = Stopped
    }


-- UPDATE

type Msg =
    Tick Time.Time
    | Click (Int, Int)

makeTick : Time.Time -> Msg
makeTick t = Tick t

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Tick time ->
            ( { model | grid = evolveGrid model.grid}
            , Cmd.none)

        Click (x, y) ->
            case model.state of
                Stopped ->
                    ( { model
                      | grid = G.mutate (x, y) invert model.grid
                      }
                    , Cmd.none)
                Playing ->
                    ( model
                    , Cmd.none)

evolveModel : Model -> Model
evolveModel m = { m | grid = evolveGrid m.grid}

evolveGrid : Grid Cell -> Grid Cell
evolveGrid g = G.indexedMap (evolveCell g) g

evolveCell : Grid Cell -> G.Index -> Cell -> Cell
evolveCell g index cell =
    let
        neighbours = G.neighbours index g
        numLive = L.length <| L.filter ((==) Live) neighbours
        numDead = L.length <| L.filter ((==) Dead) neighbours
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

-- VIEW

viewTable : Model -> Html Msg
viewTable model =
    let
        row y r = tr [] <| L.indexedMap (\x -> tdFor (x, y)) r
        tdFor (x, y) cell =
            td
                [ onClick (Click (x, y))
                , style <| tdStyle cell
                ]
                []
    in
        table [] (L.indexedMap row <| G.toLists model.grid)

view : Model -> Html Msg
view = viewTable

tdStyle : Cell -> List (String, String)
tdStyle cell =
    [ ("width", globals.width)
    , ("height", globals.width)
    , ("background", case cell of
        Live -> "white"
        Dead -> "black")
    , ("border", "1px solid gray")
    ]
