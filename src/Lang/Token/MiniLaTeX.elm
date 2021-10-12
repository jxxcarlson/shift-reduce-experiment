module Lang.Token.MiniLaTeX exposing (tokenParser)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Lang.Token.Common as Common exposing (TokenParser)
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing (Parser)


tokenParser : Int -> Parser Context Problem Token
tokenParser start =
    Parser.oneOf
        [ Common.textParser MiniLaTeX start
        , Common.mathParser start
        , macroParser start
        , Common.symbolParser start '{'
        , Common.symbolParser start '}'
        ]


macroParser : Int -> TokenParser
macroParser start =
    ParserTools.text (\c -> c == '\\') (\c -> c /= '{')
        |> Parser.map (\data -> FunctionName (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin - 1 })
