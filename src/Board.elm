module Board exposing (..)

import Random.Pcg exposing (Generator, Seed, sample, step)


-- # Types


type alias Board =
    List Block


type alias Block =
    ( Int, Int )


type alias Location =
    ( Int, Int )


type alias Piece =
    { location : Location
    , pieceType : PieceType
    , rotation : Rotation
    }


type PieceType
    = I
    | O
    | T
    | S
    | Z
    | J
    | L


type Rotation
    = Base
    | Quarter
    | Half
    | ThreeQuarters


type Key
    = Down
    | Left
    | Right
    | Rotate
    | Other


freshPiece : Seed -> ( Piece, Seed )
freshPiece seed =
    let
        ( pieceType, newSeed ) =
            step pieceTypeGenerator seed
    in
    ( { location = ( 3, 19 )
      , rotation = Base
      , pieceType = pieceType
      }
    , newSeed
    )


pieceTypeGenerator : Generator PieceType
pieceTypeGenerator =
    sample [ I, O, T, S, Z, J, L ]
        |> Random.Pcg.map (Maybe.withDefault T)



-- # Piece information.


x : Piece -> Int
x piece =
    Tuple.first piece.location


y : Piece -> Int
y piece =
    Tuple.second piece.location


rawBlockCoordinates : Piece -> List Block
rawBlockCoordinates { location, pieceType, rotation } =
    let
        pieceBlocks =
            blocks pieceType rotation

        ( x, y ) =
            location
    in
    List.map (\( blockX, blockY ) -> ( blockX + x, blockY + y )) pieceBlocks


isOffBoard : Piece -> Bool
isOffBoard piece =
    lowestX piece < 0 || highestX piece > 9 || lowestY piece < 0


hasConflicts : Piece -> Board -> Bool
hasConflicts piece board =
    let
        rawPieceBlocks =
            rawBlockCoordinates piece
    in
    (List.map
        (\block1 ->
            List.filter (\block2 -> block2 == block1) board
        )
        rawPieceBlocks
        |> List.concat
        |> (not << List.isEmpty)
    )
        || isOffBoard piece


lowestX : Piece -> Int
lowestX piece =
    let
        rawBlocks =
            rawBlockCoordinates piece
    in
    List.map Tuple.first rawBlocks
        |> List.minimum
        |> Maybe.withDefault 0


highestX : Piece -> Int
highestX piece =
    let
        rawBlocks =
            rawBlockCoordinates piece
    in
    List.map Tuple.first rawBlocks
        |> List.maximum
        |> Maybe.withDefault 9


lowestY : Piece -> Int
lowestY piece =
    let
        rawBlocks =
            rawBlockCoordinates piece
    in
    List.map Tuple.second rawBlocks
        |> List.minimum
        |> Maybe.withDefault 0


highestY : Piece -> Int
highestY piece =
    let
        rawBlocks =
            rawBlockCoordinates piece
    in
    List.map Tuple.second rawBlocks
        |> List.maximum
        |> Maybe.withDefault 18


isBottomed : Piece -> Bool
isBottomed piece =
    lowestY piece == 0



-- # Piece actions.


moveLeft : Piece -> Piece
moveLeft piece =
    { piece | location = ( x piece - 1, y piece ) }


moveRight : Piece -> Piece
moveRight piece =
    { piece | location = ( x piece + 1, y piece ) }


moveDown : Piece -> Piece
moveDown piece =
    { piece | location = ( x piece, y piece - 1 ) }


rotate : Piece -> Piece
rotate piece =
    let
        newRotation =
            case piece.rotation of
                Base ->
                    Quarter

                Quarter ->
                    Half

                Half ->
                    ThreeQuarters

                ThreeQuarters ->
                    Base
    in
    { piece | rotation = newRotation }


nextPiece : Piece -> Board -> Key -> Piece
nextPiece piece board direction =
    let
        newPiece =
            case direction of
                Down ->
                    moveDown piece

                Left ->
                    moveLeft piece

                Right ->
                    moveRight piece

                Rotate ->
                    rotate piece

                Other ->
                    piece
    in
    if hasConflicts newPiece board then
        piece
    else
        newPiece


transferPieceToBoard : Piece -> Board -> Board
transferPieceToBoard piece board =
    List.concat [ rawBlockCoordinates piece, board ]


checkForPoints : Board -> ( Board, Int )
checkForPoints board =
    ( board, 0 )


blocks : PieceType -> Rotation -> List Block
blocks pieceType rotation =
    case pieceType of
        I ->
            case rotation of
                Base ->
                    -- ....
                    -- oooo
                    -- ....
                    -- ....
                    [ ( 0, 2 ), ( 1, 2 ), ( 2, 2 ), ( 3, 2 ) ]

                Quarter ->
                    -- ..o.
                    -- ..o.
                    -- ..o.
                    -- ..o.
                    [ ( 2, 0 ), ( 2, 1 ), ( 2, 2 ), ( 2, 3 ) ]

                Half ->
                    -- ....
                    -- ....
                    -- oooo
                    -- ....
                    [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 3, 1 ) ]

                ThreeQuarters ->
                    -- .o..
                    -- .o..
                    -- .o..
                    -- .o..
                    [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]

        O ->
            case rotation of
                _ ->
                    -- .oo.
                    -- .oo.
                    -- ....
                    [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]

        T ->
            case rotation of
                Base ->
                    -- .o.
                    -- ooo
                    -- ...
                    [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ]

                Quarter ->
                    -- .o.
                    -- .oo
                    -- .o.
                    [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ]

                Half ->
                    -- ...
                    -- ooo
                    -- .o.
                    [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]

                ThreeQuarters ->
                    -- .o.
                    -- oo.
                    -- .o.
                    [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

        S ->
            case rotation of
                Base ->
                    -- .oo
                    -- oo.
                    -- ...
                    [ ( 0, 1 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ]

                Quarter ->
                    -- .o.
                    -- .oo
                    -- ..o
                    [ ( 1, 1 ), ( 1, 2 ), ( 2, 0 ), ( 2, 1 ) ]

                Half ->
                    -- ...
                    -- .oo
                    -- oo.
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 2, 1 ) ]

                ThreeQuarters ->
                    -- o..
                    -- oo.
                    -- .o.
                    [ ( 0, 1 ), ( 0, 2 ), ( 1, 0 ), ( 1, 1 ) ]

        Z ->
            case rotation of
                Base ->
                    -- oo.
                    -- .oo
                    -- ...
                    [ ( 0, 2 ), ( 1, 1 ), ( 1, 2 ), ( 2, 1 ) ]

                Quarter ->
                    -- ..o
                    -- .oo
                    -- .o.
                    [ ( 1, 0 ), ( 1, 1 ), ( 2, 1 ), ( 2, 2 ) ]

                Half ->
                    -- ...
                    -- oo.
                    -- .oo
                    [ ( 0, 1 ), ( 1, 0 ), ( 1, 1 ), ( 2, 0 ) ]

                ThreeQuarters ->
                    -- .o.
                    -- oo.
                    -- o..
                    [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 1, 2 ) ]

        J ->
            case rotation of
                Base ->
                    -- o..
                    -- ooo
                    -- ...
                    [ ( 0, 1 ), ( 0, 2 ), ( 1, 1 ), ( 2, 1 ) ]

                Quarter ->
                    -- .oo
                    -- .o.
                    -- .o.
                    [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ]

                Half ->
                    -- ...
                    -- ooo
                    -- ..o
                    [ ( 0, 1 ), ( 1, 1 ), ( 2, 0 ), ( 2, 1 ) ]

                ThreeQuarters ->
                    -- .o.
                    -- .o.
                    -- oo.
                    [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]

        L ->
            case rotation of
                Base ->
                    -- ..o
                    -- ooo
                    -- ...
                    [ ( 0, 1 ), ( 1, 1 ), ( 2, 1 ), ( 2, 2 ) ]

                Quarter ->
                    -- .o.
                    -- .o.
                    -- .oo
                    [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 2 ) ]

                Half ->
                    -- ...
                    -- ooo
                    -- o..
                    [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( 2, 1 ) ]

                ThreeQuarters ->
                    -- oo.
                    -- .o.
                    -- .o.
                    [ ( 0, 2 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ) ]
