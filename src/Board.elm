module Board exposing (..)

import Random.Pcg exposing (Generator, Seed, sample, step)


-- # Constants


width : Int
width =
    10


height : Int
height =
    20



-- # Types


type alias Board =
    List Block


type alias Positioned a =
    { a
        | x : Int
        , y : Int
    }


type alias Block =
    Positioned
        { color : Color
        }


type alias Piece =
    Positioned
        { pieceType : PieceType
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


type Color
    = Cyan
    | Blue
    | Orange
    | Yellow
    | Green
    | Purple
    | Red


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



-- # Initialization.


freshPiece : Seed -> ( Piece, Seed )
freshPiece seed =
    let
        ( pieceType, newSeed ) =
            step pieceTypeGenerator seed
    in
    ( { x = 3
      , y = 19
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


blocksFromPiece : Piece -> List Block
blocksFromPiece { x, y, pieceType, rotation } =
    let
        pieceBlocks =
            blocks pieceType rotation
    in
    List.map
        (\block ->
            { block
                | x = block.x + x
                , y = block.y + y
            }
        )
        pieceBlocks


isOffBoard : Piece -> Bool
isOffBoard piece =
    lowestX piece < 0 || highestX piece > (width - 1) || lowestY piece < 0


hasConflicts : Piece -> Board -> Bool
hasConflicts piece board =
    let
        pieceBlocks : List Block
        pieceBlocks =
            blocksFromPiece piece
    in
    (List.map
        (\pieceBlock ->
            List.filter
                (\boardBlock ->
                    boardBlock.x == pieceBlock.x && boardBlock.y == pieceBlock.y
                )
                board
        )
        pieceBlocks
        |> List.concat
        |> (not << List.isEmpty)
    )
        || isOffBoard piece



-- findMatchingBlocks : Board -> Location -> List Location
-- findMatchingBlocks board location =
--     List.filter (\)


lowestX : Piece -> Int
lowestX piece =
    let
        pieceBlocks =
            blocksFromPiece piece
    in
    List.map .x pieceBlocks
        |> List.minimum
        |> Maybe.withDefault 0


highestX : Piece -> Int
highestX piece =
    let
        pieceBlocks =
            blocksFromPiece piece
    in
    List.map .x pieceBlocks
        |> List.maximum
        |> Maybe.withDefault (width - 1)


lowestY : Piece -> Int
lowestY piece =
    let
        pieceBlocks =
            blocksFromPiece piece
    in
    List.map .y pieceBlocks
        |> List.minimum
        |> Maybe.withDefault 0


isBottomed : Piece -> Bool
isBottomed piece =
    lowestY piece == 0



-- # Piece actions.


moveLeft : Piece -> Piece
moveLeft ({ x } as piece) =
    { piece | x = x - 1 }


moveRight : Piece -> Piece
moveRight ({ x } as piece) =
    { piece | x = x + 1 }


moveDown : Piece -> Piece
moveDown ({ y } as piece) =
    { piece | y = y - 1 }


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
    List.concat [ blocksFromPiece piece, board ]



-- # Point calculation and board manipulation.


checkForPoints : Board -> ( Board, Int )
checkForPoints board =
    checkBoardForPoint board


checkBoardForPoint : Board -> ( Board, Int )
checkBoardForPoint board =
    let
        rowIndicies : List Int
        rowIndicies =
            List.range 0 (height - 1)
    in
    List.foldr checkRowForPoint ( board, 0 ) rowIndicies


checkRowForPoint : Int -> ( Board, Int ) -> ( Board, Int )
checkRowForPoint rowIndex ( board, pointCount ) =
    let
        rowAtIndex : List Block
        rowAtIndex =
            List.filter (\block -> block.y == rowIndex) board

        isFullRow : Bool
        isFullRow =
            List.length rowAtIndex == width
    in
    if isFullRow then
        ( moveRowsDownAboveIndex (removeRow board rowIndex) rowIndex, pointCount + 1 )
    else
        ( board, pointCount )


removeRow : Board -> Int -> Board
removeRow board rowIndex =
    List.filter (\block -> block.y /= rowIndex) board


moveRowsDownAboveIndex : Board -> Int -> Board
moveRowsDownAboveIndex board rowIndex =
    let
        rowsAboveRowIndex : List Block
        rowsAboveRowIndex =
            List.filter (\block -> block.y > rowIndex) board

        moveBlockDown : Block -> Block
        moveBlockDown ({ y } as block) =
            { block | y = y - 1 }
    in
    List.map moveBlockDown rowsAboveRowIndex



-- # Piece colors and structures.


color : PieceType -> Color
color pieceType =
    case pieceType of
        I ->
            Cyan

        J ->
            Blue

        L ->
            Orange

        O ->
            Yellow

        S ->
            Green

        T ->
            Purple

        Z ->
            Red


blocksFromTuples : Color -> List ( Int, Int ) -> List Block
blocksFromTuples color coords =
    List.map (\( x, y ) -> { x = x, y = y, color = color }) coords


blocks : PieceType -> Rotation -> List Block
blocks pieceType rotation =
    case pieceType of
        I ->
            blocksFromTuples Cyan <|
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
            blocksFromTuples Yellow <|
                case rotation of
                    _ ->
                        -- .oo.
                        -- .oo.
                        -- ....
                        [ ( 1, 1 ), ( 1, 2 ), ( 2, 1 ), ( 2, 2 ) ]

        T ->
            blocksFromTuples Purple <|
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
            blocksFromTuples Green <|
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
            blocksFromTuples Red <|
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
            blocksFromTuples Blue <|
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
            blocksFromTuples Orange <|
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
                        [ ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 2, 0 ) ]

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
