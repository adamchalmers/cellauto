module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class, value, placeholder, id)
import Html.Events exposing (onClick, onInput)
import List as L
import Time

import GameOfLife as Rules exposing (Cell)
import Grid as G

-- MODEL

type PlayState = Stopped | Playing | Paused
type alias State =
    { play: PlayState
    , boardCode: Bool
    }

type alias CellGrid = G.Grid Cell

type alias Model =
    { state: State
    , cellgrid: CellGrid
    , startGrid: CellGrid
    , rounds: Int
    , msg: String
    , pastedCode : String
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
    ( { state = {play = Stopped, boardCode = True}
      , cellgrid = initGrid
      , startGrid = initGrid
      , msg = ""
      , pastedCode = ""
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
    | ToggleBoardCode
    | LoadBoard
    | CodePasted String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        -- Starts evolution of the board according to game rules, disables board editing.
        Play ->
            ( { model
              | state = {play = Playing, boardCode = model.state.boardCode}
              }
            , Cmd.none)

        -- Pauses evolution.
        Pause ->
            ( { model
              | state = {play = Paused, boardCode = model.state.boardCode}
              }
            , Cmd.none)

        -- Resets the board (undoing all evolution) and enables board editing.
        Stop ->
            ( { model
              | state = {play = Stopped, boardCode = model.state.boardCode}
              , cellgrid = model.startGrid
              , rounds = 0
              }
            , Cmd.none)

        -- When time ticks forward, evolve the model according to the cellular automaton rules.
        Tick t ->

            case model.state.play of

                Playing ->
                    ( { model
                      | cellgrid = G.indexedMap (\index c -> Rules.evolveCell c (G.neighbours index model.cellgrid)) model.cellgrid
                      , rounds = model.rounds + 1
                      }
                    , Cmd.none)

                _ -> (model, Cmd.none)

        -- If the game is stopped, allow the player to modify the starting automaton configuration. Only allowed while game is stopped.
        Click (x, y) ->

            case model.state.play of

                Stopped ->
                    ( { model
                      | cellgrid = G.mutate (x, y) Rules.onClick model.cellgrid
                      , startGrid = G.mutate (x, y) Rules.onClick model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Add a row/column. Only allowed while game is stopped.
        IncSize ->
            case model.state.play of

                Stopped ->
                    ( { model
                      | cellgrid = G.incSize Rules.defaultCell model.cellgrid
                      , startGrid = G.incSize Rules.defaultCell model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Add a row/column. Only allowed while game is stopped.
        DecSize ->
            case model.state.play of

                Stopped ->
                    ( { model
                      | cellgrid = G.decSize model.cellgrid
                      , startGrid = G.decSize model.cellgrid
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Clear the board by setting all cells to their default value.
        Clear ->
            case model.state.play of

                Stopped ->
                    ( { model
                      | cellgrid = G.init Rules.defaultCell (G.length model.startGrid) (G.length model.startGrid)
                      , startGrid = G.init Rules.defaultCell (G.length model.startGrid) (G.length model.startGrid)
                      }
                    , Cmd.none)
                _ -> (model, Cmd.none)

        -- Encodes the user's initial board design into a string and logs it in the console.
        ToggleBoardCode ->
            ({model | state = {boardCode = not model.state.boardCode, play = model.state.play}}, Cmd.none)

        LoadBoard ->
            case model.state.play of

            Stopped ->
                case G.unpickle Rules.defaultCell Rules.unpickleCell model.pastedCode of
                    Err s ->
                        ( { model
                          | msg = Debug.log "Unpickling: " s
                          }
                        , Cmd.none)
                    Ok board ->
                        ( { model
                          | msg = "no error"
                          , cellgrid = board
                          , startGrid = board
                          }
                        , Cmd.none)


            _ -> (model, Cmd.none)

        CodePasted s ->
            ( { model | pastedCode = s }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ table []
            [ tr [] (buttonsFor model)
            , tr [] [codeLoadSaveGui model]
            , tr [] [ text ("Time: " ++ (toString model.rounds))]
            ]
        , table [ id "cellautogrid" ] (cellGridRows model.cellgrid)
        ]

cellGridRows : G.Grid Cell -> List (Html Msg)
-- A list of <tr> elements for displaying the CA grid as an HTML table.
cellGridRows grid =
    let
        row y r = tr [] <| L.indexedMap (\x -> tdFor (x, y)) r
        tdFor (x, y) cell =
            td [ onClick (Click (x, y)), class (Rules.cssClass cell)] []
    in
        L.indexedMap row <| G.toLists grid

buttonsFor : Model -> List (Html Msg)
-- The state determines which GUI controls to show.
buttonsFor m =
    let
        stop = button [onClick Stop] [text "Stop"]
        play = button [onClick Play] [text "Play"]
        pause = button [onClick Pause] [text "Pause"]
        plus = button [onClick IncSize] [text "+ Row"]
        minus = button [onClick DecSize] [text "- Row"]
        clear = button [onClick Clear] [text "Clear board"]
    in
        case m.state.play of
            Stopped -> [play, plus, minus, clear]
            Playing -> [pause, stop]
            Paused -> [play, stop]

codeLoadSaveGui : Model -> Html Msg
codeLoadSaveGui m =
        let
            boardCodeVal = G.pickle Rules.pickleCell m.cellgrid
        in
            table []
            [ tr [] [td [] [text "Your board: "], td [] [input [value boardCodeVal] []]]
            , tr [] [td [] [text "Load board: "], td [] [input [placeholder "Paste board code here", onInput CodePasted] [], button [onClick LoadBoard] [text "Load"]]]
            ]


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
