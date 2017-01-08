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


type Direction
    = Down
    | Up
    | Left
    | Right


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


move : Piece -> Board -> Direction -> Piece
move piece board direction =
    case direction of
        Down ->
            -- let rawCells = rawPieceCells piece,
            --     nextCells = List.map (\c -> {c | y = c.y - 1}) rawCells
            -- conflicts =
            let
                ( x, y ) =
                    piece.location

                newY =
                    if y - 1 >= 0 then
                        y - 1
                    else
                        y
            in
                { piece | location = ( x, newY ) }

        _ ->
            piece


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
                    [ ( 1, 1 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ]

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
