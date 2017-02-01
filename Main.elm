module Main exposing (..)

import Html exposing (Html, program)
import Time
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Grid as G
import List as L

-- MODEL

type State = Stopped | Playing | Paused

type alias CellGrid = G.Grid Cell

type alias Model =
    { state: State
    , cellgrid: CellGrid
    , width: Int
    , startGrid: CellGrid
    }

globals =
    { startWidth = 8
    , cellWidth = "10px"
    }

initGrid = G.initAs (\(i,j) -> if (i%2==0) then Live else Dead) globals.startWidth globals.startWidth

init : (Model, Cmd Msg)
init =
    ( { state = Stopped
      , cellgrid = initGrid
      , startGrid = initGrid
      , width = globals.startWidth}
    , Cmd.none
    )

-- UPDATE

type Msg =
    Tick Time.Time
    | Click (Int, Int)
    | Play
    | Pause
    | Stop
    | IncSize
    | DecSize

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        Play ->
            ( { model
              | state = Playing
              }
            , Cmd.none)

        Pause ->
            ( { model
              | state = Paused
              }
            , Cmd.none)

        Stop ->
            ( { model
              | state = Stopped
              , cellgrid = model.startGrid
              }
            , Cmd.none)

        -- When time ticks forward, evolve the model according to the cellular automaton rules.
        Tick t ->

            case model.state of

                Playing ->
                    ( { model
                      | cellgrid = evolve model.cellgrid
                      }
                    , Cmd.none)

                _ -> (model, Cmd.none)

        -- If the game is stopped, allow the player to modify the starting automaton configuration. Only allowed while game is stopped.
        Click (x, y) ->

            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.mutate (x, y) invert model.cellgrid
                      , startGrid = G.mutate (x, y) invert model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Add a row/column. Only allowed while game is stopped.
        IncSize ->
            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.incSize Live model.cellgrid
                      , startGrid = G.incSize Live model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Add a row/column. Only allowed while game is stopped.
        DecSize ->
            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.decSize model.cellgrid
                      , startGrid = G.decSize model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div [] <|
        (buttonsFor model.state) ++
        [ cellGridTable model.cellgrid
        ]

cellGridTable : CellGrid -> Html Msg
-- Visualise the cellular automaton grid as an HTML table.
cellGridTable grid =
    let
        row y r = tr [] <| L.indexedMap (\x -> tdFor (x, y)) r
        tdFor (x, y) cell =
            td
                [ onClick (Click (x, y))
                , style <| tdStyle cell
                ]
                []
    in
        table [] (L.indexedMap row <| G.toLists grid)

tdStyle : Cell -> List (String, String)
tdStyle cell =
    [ ("width", globals.cellWidth)
    , ("height", globals.cellWidth)
    , ("background", case cell of
        Live -> "white"
        Dead -> "black")
    , ("border", "1px solid gray")
    ]

buttonsFor : State -> List (Html Msg)
-- The state determines which GUI controls to show.
buttonsFor state =
    let
        stop = button [onClick Stop] [text "Stop"]
        play = button [onClick Play] [text "Play"]
        pause = button [onClick Pause] [text "Pause"]
        plus = button [onClick IncSize] [text "+ Row"]
        minus = button [onClick DecSize] [text "- Row"]
    in case state of
        Stopped -> [play, plus, minus]
        Playing -> [pause, stop]
        Paused -> [play, stop]

-- WIRING

subs : Model -> Sub Msg
subs model = Time.every Time.second Tick

main : Program Never Model Msg
main = program
    { init = init
    , view = view
    , update = update
    , subscriptions = subs
    }

-- CELLULAR AUTOMATON LOGIC

type Cell = Live | Dead

invert : Cell -> Cell
invert c =
    case c of
        Live -> Dead
        Dead -> Live

evolve : CellGrid -> CellGrid
evolve g = G.indexedMap (evolveCell g) g

evolveCell : CellGrid -> (Int, Int) -> Cell -> Cell
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