module Board exposing (..)


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


type Movement
    = Down
    | Left
    | Right
    | Rotate


freshPiece : PieceType -> Piece
freshPiece pieceType =
    { location = ( 3, 19 )
    , rotation = Base
    , pieceType = pieceType
    }


rawBlockCoordinates : Piece -> List Block
rawBlockCoordinates { location, pieceType, rotation } =
    let
        pieceBlocks =
            blocks pieceType rotation

        ( x, y ) =
            location
    in
        List.map (\( blockX, blockY ) -> ( blockX + x, blockY + y )) pieceBlocks


hasConflicts : Piece -> Board -> Bool
hasConflicts piece board =
    let
        rawPieceBlocks =
            rawBlockCoordinates piece
    in
        List.map
            (\block1 ->
                List.filter (\block2 -> block2 == block1) board
            )
            rawPieceBlocks
            |> List.concat
            |> (not << List.isEmpty)


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


isBottomed : Piece -> Bool
isBottomed piece =
    lowestY piece == 0


isLeftmost : Piece -> Bool
isLeftmost piece =
    lowestX piece == 0


isRightmost : Piece -> Bool
isRightmost piece =
    highestX piece == 9


move : Piece -> Board -> Movement -> ( Piece, Board )
move piece board direction =
    case direction of
        Down ->
            let
                ( x, y ) =
                    piece.location

                newPiece =
                    { piece | location = ( x, y - 1 ) }

                conflicts =
                    hasConflicts newPiece board
            in
                if conflicts || isBottomed piece then
                    ( piece, board )
                else
                    ( newPiece, board )

        Left ->
            let
                ( x, y ) =
                    piece.location

                newPiece =
                    { piece | location = ( x - 1, y ) }

                conflicts =
                    hasConflicts newPiece board
            in
                if conflicts || isLeftmost piece then
                    ( piece, board )
                else
                    ( newPiece, board )

        Right ->
            let
                ( x, y ) =
                    piece.location

                newPiece =
                    { piece | location = ( x + 1, y ) }

                conflicts =
                    hasConflicts newPiece board
            in
                if conflicts || isRightmost piece then
                    ( piece, board )
                else
                    ( newPiece, board )

        Rotate ->
            let
                { rotation } =
                    piece

                newRotation =
                    case rotation of
                        Base ->
                            Quarter

                        Quarter ->
                            Half

                        Half ->
                            ThreeQuarters

                        ThreeQuarters ->
                            Base

                low =
                    lowestY piece
            in
                if isBottomed piece then
                    ( piece, board )
                else
                    ( { piece | rotation = newRotation }, board )


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
                    [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]

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
