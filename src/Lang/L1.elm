module Lang.L1 exposing (codeParser, functionNameParser)

import Markup.Debugger exposing (..)
import Markup.Error exposing (..)
import Markup.L1 exposing (makeLoc)
import Markup.Lang exposing (Lang(..))
import Markup.ParserTools as ParserTools
import Markup.Token exposing (Token(..))
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type alias TokenParser =
    Parser Context Problem Token


codeParser : Int -> TokenParser
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin - 1 })


functionNameParser : Int -> TokenParser
functionNameParser start =
    ParserTools.textWithEndSymbol " " (\c -> c == '[') (\c -> c /= ' ')
        |> Parser.map (\data -> FunctionName data.content { begin = start, end = start + data.end - data.begin - 1 })
