module Main exposing (..)

import Board exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Keyboard exposing (KeyCode)
import Random.Pcg exposing (Seed, initialSeed)
import Time exposing (Time, second)


-- # Randomness


type alias Flags =
    { randomSeed : Int
    }



-- # Main


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- # Model


type alias Model =
    { score : Int
    , board : List Block
    , seed : Seed
    , currentPiece : Piece
    , nextPiece : Piece
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            initialSeed flags.randomSeed

        ( currentPiece, seed2 ) =
            freshPiece seed

        ( nextPiece, seed3 ) =
            freshPiece seed2
    in
    ( { score = 0
      , board = []
      , seed = seed3
      , currentPiece = currentPiece
      , nextPiece = nextPiece
      }
    , Cmd.none
    )



-- # Messages


type Msg
    = Tick Time
    | KeyPress KeyCode



-- # Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            step model Down

        KeyPress keyCode ->
            let
                key =
                    if keyCode == 87 || keyCode == 38 then
                        Rotate
                    else if keyCode == 83 || keyCode == 40 then
                        Down
                    else if keyCode == 65 || keyCode == 37 then
                        Left
                    else if keyCode == 68 || keyCode == 39 then
                        Right
                    else
                        Other
            in
            step model key


step : Model -> Key -> ( Model, Cmd Msg )
step model key =
    let
        { currentPiece, board, seed, score } =
            model

        steppedPiece =
            nextPiece currentPiece board key
    in
    if steppedPiece == currentPiece && key == Down then
        let
            updatedBoard =
                transferPieceToBoard currentPiece board

            ( scoredBoard, points ) =
                checkForPoints updatedBoard

            ( nextPiece, newSeed ) =
                freshPiece seed
        in
        ( { model
            | currentPiece = model.nextPiece
            , nextPiece = nextPiece
            , seed = newSeed
            , board = scoredBoard
            , score = score + points
          }
        , Cmd.none
        )
    else
        ( { model | currentPiece = steppedPiece }, Cmd.none )



-- # Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick
        , Keyboard.downs KeyPress
        ]



-- # View


view : Model -> Html Msg
view model =
    let
        emptyBlockViews =
            List.map emptyBlockView (List.range 1 (Board.width * Board.height))

        boardAndPieceBlocks =
            List.append model.board <|
                blocksFromPiece model.currentPiece
    in
    div [ class "Game" ]
        [ div [ class "Board" ] <|
            List.append emptyBlockViews <|
                List.map blockView boardAndPieceBlocks
        , div [ class "ScoreAndNextPiece" ]
            [ div [ class "Score" ]
                [ span [ class "Score-Label" ] [ text "Score" ]
                , span [ class "Score-Value" ] [ text <| toString model.score ]
                ]
            , div [ class "NextPiece" ]
                [ span [ class "NextPiece-Label" ] [ text "Next" ]
                , div [ class "NextPiece-Piece" ] <|
                    List.map
                        nextBlockView
                    <|
                        blocks model.nextPiece.pieceType Base
                ]
            ]
        ]


blockView : Block -> Html Msg
blockView { x, y, color } =
    div
        [ class <| "Block Block--piece Block--" ++ (toString color |> String.toLower)
        , style
            [ ( "left", toString <| x * 32 )
            , ( "bottom", toString <| y * 32 )
            ]
        ]
        []


nextBlockView : Block -> Html Msg
nextBlockView { x, y, color } =
    div
        [ class <| "Block Block--piece Block--nextPiece Block--" ++ (toString color |> String.toLower)
        , style
            [ ( "left", toString <| x * 16 )
            , ( "bottom", toString <| y * 16 )
            ]
        ]
        []


emptyBlockView : a -> Html Msg
emptyBlockView _ =
    div [ class "Block Block--empty" ] []
