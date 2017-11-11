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
    let
        { piece, board } =
            model
    in
    case msg of
        Tick time ->
            let
                newPiece =
                    nextPiece piece board Down
            in
            if newPiece == piece then
                let
                    ( newPiece, newSeed ) =
                        freshPiece model.seed

                    updatedBoard =
                        transferPieceToBoard piece board

                    ( scoredBoard, points ) =
                        checkForPoints updatedBoard
                in
                ( { model
                    | piece = newPiece
                    , seed = newSeed
                    , board = scoredBoard
                    , score = model.score + points
                  }
                , Cmd.none
                )
            else
                ( { model | piece = newPiece }, Cmd.none )

        KeyPress keyCode ->
            -- TODO: Dry.
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

                newPiece =
                    nextPiece piece board key
            in
            if newPiece == piece && key == Down then
                let
                    ( newPiece, newSeed ) =
                        freshPiece model.seed

                    updatedBoard =
                        transferPieceToBoard piece board

                    ( scoredBoard, points ) =
                        checkForPoints updatedBoard
                in
                ( { model
                    | piece = newPiece
                    , seed = newSeed
                    , board = scoredBoard
                    , score = model.score + points
                  }
                , Cmd.none
                )
            else if newPiece == piece then
                ( model, Cmd.none )
            else
                ( { model | piece = newPiece }, Cmd.none )



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
    div [ class "Game" ]
        [ div [ class "Board" ] <|
            List.map blockView <|
                List.append model.board <|
                    rawBlockCoordinates model.piece
        , div [ class "Score" ]
            [ span [] [ text <| toString model.score ]
            , div [ class "NextPiece" ] []
            ]
        ]


blockView : Block -> Html Msg
blockView ( x, y ) =
    div
        [ class "Block"
        , style
            [ ( "left", toString <| x * 32 )
            , ( "bottom", toString <| y * 32 )
            ]
        ]
        []
