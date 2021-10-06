module Block.L1.Line exposing (lineType)

import Block.Line as Line
import Markup.ParserTools as ParserTools
import Parser exposing ((|.), (|=), Parser)


lineType : String -> Line.LineType
lineType str =
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf [ Line.ordinaryLineParser [ '|' ], Line.emptyLineParser, beginVerbatimBlockParser, beginBlockParser ]


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
