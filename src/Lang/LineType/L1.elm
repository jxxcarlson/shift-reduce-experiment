module Lang.LineType.L1 exposing (lineType)

import Block.Line as Line
import Parser exposing ((|.), (|=), Parser)


lineType : String -> Line.LineType
lineType str =
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf
        [ beginItemParser
        , Line.ordinaryLineParser [ '|' ]
        , Line.emptyLineParser
        , beginVerbatimBlockParser
        , beginBlockParser
        ]


beginBlockParser : Parser Line.LineType
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "|"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= ' ')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.BeginBlock Line.RejectFirstLine s)


beginVerbatimBlockParser : Parser Line.LineType
beginVerbatimBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "||"
        |. Parser.chompIf (\c -> c == ' ')
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= ' ')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.BeginVerbatimBlock s)


beginItemParser : Parser Line.LineType
beginItemParser =
    (Parser.succeed String.slice
        |. Parser.symbol "-"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptNibbledFirstLine "item")
