module Main exposing (..)

-- Theirs.

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Keyboard exposing (KeyCode)
import Random.Pcg exposing (Generator, Seed, initialSeed, int, sample, step)
import Time exposing (Time, second)


-- Ours.

import Board exposing (..)


-- # Randomness


type alias Flags =
    { randomSeed : Int
    }


intGenerator : Generator Int
intGenerator =
    int 1 10


pieceTypeGenerator : Generator PieceType
pieceTypeGenerator =
    sample [ I, O, T, S, Z, J, L ]
        |> Random.Pcg.map (Maybe.withDefault T)



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
    , rand : PieceType
    , seed : Seed
    , piece : Piece
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        firstSeed =
            initialSeed flags.randomSeed

        ( rand, seed ) =
            step pieceTypeGenerator firstSeed
    in
        ( { score = 0
          , board = []
          , rand = rand
          , seed = seed
          , piece = freshPiece rand
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
                    ( newPiece, newBoard ) =
                        move piece board Down
                in
                    ( { model | piece = newPiece }, Cmd.none )

            KeyPress keyCode ->
                -- TODO: Dry.
                if keyCode == 87 || keyCode == 38 then
                    -- Up
                    let
                        ( newPiece, newBoard ) =
                            move piece board Rotate
                    in
                        ( { model | piece = newPiece }, Cmd.none )
                else if keyCode == 83 || keyCode == 40 then
                    -- Down
                    let
                        ( newPiece, newBoard ) =
                            move piece board Down
                    in
                        ( { model | piece = newPiece }, Cmd.none )
                else if keyCode == 65 || keyCode == 37 then
                    -- Left
                    let
                        ( newPiece, newBoard ) =
                            move piece board Left
                    in
                        ( { model | piece = newPiece }, Cmd.none )
                else if keyCode == 68 || keyCode == 39 then
                    -- Right
                    let
                        ( newPiece, newBoard ) =
                            move piece board Right
                    in
                        ( { model | piece = newPiece }, Cmd.none )
                else
                    ( model, Cmd.none )



-- # Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every second Tick
        , Keyboard.downs KeyPress
        ]



-- # View


columnStyle : Int -> Attribute msg
columnStyle i =
    style
        [ ( "position", "absolute" )
        , ( "left", toString <| i * 64 - 32 )
        ]


view : Model -> Html Msg
view model =
    div [ class "Game" ]
        [ div [ class "Board" ]
            <| List.map blockView
            <| List.append model.board
            <| rawBlockCoordinates model.piece
          -- [ div [ class "Columns" ]
          --     <| List.map (\i -> div [ class "Column", columnStyle i ] [])
          --     <| List.range 1 5
          -- ]
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
