module Lang.Token.Common exposing (TokenParser, mathParser, symbolParser, textParser)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing (Parser)


l1LanguageChars =
    [ '[', ']', '`', '$' ]


miniLaTeXLanguageChars =
    [ '{', '}', '\\', '$' ]


markdownLanguageChars =
    [ '*', '_', '`', '$', '#' ]


type alias TokenParser =
    Parser Context Problem Token


textParser : Lang -> Int -> TokenParser
textParser lang start =
    case lang of
        L1 ->
            ParserTools.text (\c -> not <| List.member c l1LanguageChars) (\c -> not <| List.member c l1LanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin - 1 })

        MiniLaTeX ->
            ParserTools.text (\c -> not <| List.member c miniLaTeXLanguageChars) (\c -> not <| List.member c miniLaTeXLanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin - 1 })

        Markdown ->
            ParserTools.text (\c -> not <| List.member c markdownLanguageChars) (\c -> not <| List.member c markdownLanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin - 1 })


symbolParser : Int -> Char -> TokenParser
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin - 1 })


mathParser : Int -> TokenParser
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Verbatim "math" (dropFirstAndLastCharacter data.content) { begin = start, end = start + data.end - data.begin - 1 })


dropFirstAndLastCharacter str =
    String.slice 1 (String.length str - 1) str
