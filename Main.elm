module Main exposing (..)

import Html exposing (Html, program, div, text, button, tr, td, table)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import List as L
import Time

import GameOfLife as Rules exposing (Cell)
import Grid as G

-- MODEL

type State = Stopped | Playing | Paused

type alias CellGrid = G.Grid Cell

type alias Model =
    { state: State
    , cellgrid: CellGrid
    , startGrid: CellGrid
    , rounds: Int
    }

globals =
    { numRows = 30
    , cellWidth = "10px"
    }

initGrid : CellGrid
initGrid =
    G.initAs Rules.initBoard globals.numRows globals.numRows

init : (Model, Cmd Msg)
init =
    ( { state = Stopped
      , cellgrid = initGrid
      , startGrid = initGrid
      , rounds = 0}
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
    | Clear

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
              , rounds = 0
              }
            , Cmd.none)

        -- When time ticks forward, evolve the model according to the cellular automaton rules.
        Tick t ->

            case model.state of

                Playing ->
                    ( { model
                      | cellgrid = evolve model.cellgrid
                      , rounds = model.rounds + 1
                      }
                    , Cmd.none)

                _ -> (model, Cmd.none)

        -- If the game is stopped, allow the player to modify the starting automaton configuration. Only allowed while game is stopped.
        Click (x, y) ->

            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.mutate (x, y) Rules.onClick model.cellgrid
                      , startGrid = G.mutate (x, y) Rules.onClick model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Add a row/column. Only allowed while game is stopped.
        IncSize ->
            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.incSize Rules.defaultCell model.cellgrid
                      , startGrid = G.incSize Rules.defaultCell model.cellgrid
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

        -- Clear the board by setting all cells to their default value.
        Clear ->
            case model.state of

                Stopped ->
                    ( { model
                      | cellgrid = G.init Rules.defaultCell (G.length model.startGrid) (G.length model.startGrid)
                      , startGrid = G.init Rules.defaultCell (G.length model.startGrid) (G.length model.startGrid)
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ div [ class "buttons" ] (buttonsFor model.state)
        , div [ class "rounds" ] [ text ("Time: " ++ (toString model.rounds))]
        , cellGridTable model.cellgrid
        ]

cellGridTable : CellGrid -> Html Msg
-- Visualise the cellular automaton grid as an HTML table.
cellGridTable grid =
    let
        row y r = tr [] <| L.indexedMap (\x -> tdFor (x, y)) r
        tdFor (x, y) cell =
            td
                [ onClick (Click (x, y))
                , class (Rules.cssClass cell)
                ]
                []
    in
        table [] (L.indexedMap row <| G.toLists grid)

buttonsFor : State -> List (Html Msg)
-- The state determines which GUI controls to show.
buttonsFor state =
    let
        stop = button [onClick Stop] [text "Stop"]
        play = button [onClick Play] [text "Play"]
        pause = button [onClick Pause] [text "Pause"]
        plus = button [onClick IncSize] [text "+ Row"]
        minus = button [onClick DecSize] [text "- Row"]
        clear = button [onClick Clear] [text "Clear board"]
    in case state of
        Stopped -> [play, plus, minus, clear]
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

evolve : CellGrid -> CellGrid
evolve g =
    G.indexedMap (\index cell -> Rules.evolveCell cell (G.neighbours index g)) g
