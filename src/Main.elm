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
    , piece : Piece
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            initialSeed flags.randomSeed

        ( firstPiece, newSeed ) =
            freshPiece seed
    in
    ( { score = 0
      , board = []
      , seed = newSeed
      , piece = firstPiece
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
        { piece, board, seed, score } =
            model

        steppedPiece =
            nextPiece piece board key
    in
    if steppedPiece == piece && key == Down then
        let
            updatedBoard =
                transferPieceToBoard piece board

            ( scoredBoard, points ) =
                checkForPoints updatedBoard

            ( newPiece, newSeed ) =
                freshPiece seed
        in
        ( { model
            | piece = newPiece
            , seed = newSeed
            , board = scoredBoard
            , score = score + points
          }
        , Cmd.none
        )
    else
        ( { model | piece = steppedPiece }, Cmd.none )



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
        emptyBlocks =
            List.map emptyBlock (List.range 1 (Board.width * Board.height))
    in
    div [ class "Game" ]
        [ div [ class "Board" ] <|
            List.append emptyBlocks <|
                List.map
                    blockView
                <|
                    List.append model.board <|
                        blocksFromPiece model.piece
        , div [ class "Score" ]
            [ span [] [ text <| toString model.score ]
            , div [ class "NextPiece" ] []
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


emptyBlock : a -> Html Msg
emptyBlock _ =
    div [ class "Block Block--empty" ] []
