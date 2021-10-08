module Block.Line exposing (BlockOption(..), LineData, LineType(..), countLeadingSpaces, emptyLineParser, ordinaryLineParser)

import Parser exposing ((|.), Parser)


type LineType
    = OrdinaryLine
    | VerbatimLine
    | BlankLine
    | BeginBlock BlockOption String
    | EndBlock String
    | BeginVerbatimBlock String
    | EndVerbatimBlock String
    | Problem String


{-| In the case of the AcceptFirstLine option, the line after the block mark
is incorporated into the body of the block, e.g. TODO add example
-}
type BlockOption
    = AcceptNibbledFirstLine
    | AcceptFirstLine
    | RejectFirstLine


type alias LineData =
    { indent : Int, lineType : LineType, content : String }


countLeadingSpaces : String -> Int
countLeadingSpaces str =
    case Parser.run leadingBlanksParser str of
        Ok k ->
            k

        Err _ ->
            0


leadingBlanksParser : Parser Int
leadingBlanksParser =
    (Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\c -> c == ' ')
    )
        |> Parser.map String.length


emptyLineParser : Parser LineType
emptyLineParser =
    Parser.end |> Parser.map (\_ -> BlankLine)


ordinaryLineParser : List Char -> Parser LineType
ordinaryLineParser blockStartChars =
    Parser.chompIf (\c -> not (List.member c blockStartChars)) |> Parser.map (\_ -> OrdinaryLine)
