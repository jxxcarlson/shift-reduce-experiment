module Lang.LineType.MiniLaTeX exposing (lineType)

import Block.Line as Line
import Parser exposing ((|.), (|=), Parser)


verbatimEnvironments =
    [ "align", "equation", "mathmacro", "code", "math", "verbatim" ]


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
        , beginCodeBlockParser
        , beginBlockParser
        , endBlockParser
        , beginMathBlockParser
        , Line.ordinaryLineParser []
        , Line.emptyLineParser
        ]


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


beginBlockParser : Parser Line.LineType
beginBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\begin{"
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '}')
        |= Parser.getOffset
        |= Parser.getSource
    )
        -- |> Parser.map (\s -> Line.BeginBlock Line.RejectFirstLine s)
        |> Parser.map (\s -> mapBlock s)


beginItemParser : Parser Line.LineType
beginItemParser =
    (Parser.succeed String.slice
        |. Parser.spaces
        |. Parser.symbol "\\item "
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '\n')
        |= Parser.getOffset
        |= Parser.getSource
    )
        -- |> Parser.map (\s -> Line.BeginBlock Line.RejectFirstLine s)
        |> Parser.map (\s -> Line.BeginBlock Line.AcceptNibbledFirstLine "item")


mapBlock : String -> Line.LineType
mapBlock str =
    if List.member str verbatimEnvironments then
        Line.BeginVerbatimBlock str

    else
        Line.BeginBlock Line.RejectFirstLine str


endBlockParser : Parser Line.LineType
endBlockParser =
    (Parser.succeed String.slice
        |. Parser.symbol "\\end{"
        |= Parser.getOffset
        |. Parser.chompWhile (\c -> c /= '}')
        |= Parser.getOffset
        |= Parser.getSource
    )
        |> Parser.map (\s -> Line.EndBlock s)
