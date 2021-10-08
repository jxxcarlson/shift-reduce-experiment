module Block.Markdown.Line exposing (lineType)

import Block.Line as Line
import Parser exposing ((|.), Parser)


lineType : String -> Line.LineType
lineType str =
    case Parser.run lineTypeParser str of
        Ok type_ ->
            type_

        Err _ ->
            Line.Problem "unrecognized type"


lineTypeParser =
    Parser.oneOf
        [ beginCodeBlockParser
        , beginMathBlockParser
        , beginItemParser
        , beginHeadingParser
        , beginQuotationBlockParser
        , Line.ordinaryLineParser []
        , Line.emptyLineParser
        ]


beginItemParser : Parser Line.LineType
beginItemParser =
    (Parser.succeed String.slice
        |. Parser.symbol "-"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptNibbledFirstLine "item")


beginHeadingParser : Parser Line.LineType
beginHeadingParser =
    (Parser.succeed String.slice
        |. Parser.symbol "#"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptFirstLine "heading")


beginMathBlockParser : Parser Line.LineType
beginMathBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "$$"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock "math")


beginCodeBlockParser : Parser Line.LineType
beginCodeBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "```"
    )
        |> Parser.map (\_ -> Line.BeginVerbatimBlock "code")


beginQuotationBlockParser : Parser Line.LineType
beginQuotationBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol ">"
    )
        |> Parser.map (\_ -> Line.BeginBlock Line.AcceptNibbledFirstLine "quotation")
