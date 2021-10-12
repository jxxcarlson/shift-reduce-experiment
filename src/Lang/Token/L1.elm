module Lang.Token.L1 exposing (tokenParser)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Lang.Token.Common as Common
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing (Parser)


type alias TokenParser =
    Parser Context Problem Token


tokenParser : Int -> TokenParser
tokenParser start =
    Parser.oneOf
        [ Common.textParser L1 start
        , Common.mathParser start
        , codeParser start
        , functionNameParser start
        , Common.symbolParser start ']'
        ]


codeParser : Int -> TokenParser
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin - 1 })


functionNameParser : Int -> TokenParser
functionNameParser start =
    ParserTools.textWithEndSymbol " " (\c -> c == '[') (\c -> c /= ' ')
        |> Parser.map (\data -> FunctionName data.content { begin = start, end = start + data.end - data.begin - 1 })
