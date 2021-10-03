module Tokenizer exposing (Lang(..), get, markedTextParser, tokenParser)

import Error exposing (..)
import Parser.Advanced as Parser exposing (Parser)
import ParserTools
import Token exposing (Loc, Token(..))


get : Lang -> Int -> String -> Result (List (Parser.DeadEnd Context Problem)) Token
get lang start input =
    Parser.run (tokenParser lang start) input


type Lang
    = L1
    | MiniLaTeX
    | Markdown


l1LanguageChars =
    [ '[', ']', '`', '$' ]


miniLaTeXLanguageChars =
    [ '{', '}', '\\', '$' ]


markdownLanguageChars =
    [ '*', '_', '`', '$', '[', ']', '(', ')', '#' ]


{-|

    > Tokenizer.run "Test: [i [j foo bar]]"
      Ok [Text ("Test: "),Symbol "[",Text ("i "),Symbol "[",Text ("j foo bar"),Symbol "]",Symbol "]"]

-}
tokenParser lang start =
    case lang of
        L1 ->
            Parser.oneOf [ textParser lang start, mathParser start, codeParser start, symbolParser start '[', symbolParser start ']' ]

        MiniLaTeX ->
            Parser.oneOf [ textParser lang start, mathParser start, macroParser start, symbolParser start '{', symbolParser start '}' ]

        Markdown ->
            Parser.oneOf
                [ markedTextParser start "strong" '*' '*'
                , markedTextParser start "italic" '_' '_'
                , markedTextParser start "arg" '(' ')'
                , markedTextParser start "annotation" '[' ']'
                , textParser lang start
                ]


textParser : Lang -> Int -> Parser Context Problem Token
textParser lang start =
    case lang of
        L1 ->
            ParserTools.text (\c -> not <| List.member c l1LanguageChars) (\c -> not <| List.member c l1LanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin })

        MiniLaTeX ->
            ParserTools.text (\c -> not <| List.member c miniLaTeXLanguageChars) (\c -> not <| List.member c miniLaTeXLanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin })

        Markdown ->
            ParserTools.text (\c -> not <| List.member c markdownLanguageChars) (\c -> not <| List.member c markdownLanguageChars)
                |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin })


macroParser : Int -> Parser Context Problem Token
macroParser start =
    ParserTools.text (\c -> c == '\\') (\c -> c /= '{')
        |> Parser.map (\data -> FunctionName (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin })


mathParser : Int -> Parser Context Problem Token
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Verbatim "math" data.content { begin = start, end = start + data.end - data.begin })


markedTextParser : Int -> String -> Char -> Char -> Parser Context Problem Token
markedTextParser start mark begin end =
    ParserTools.text (\c -> c == begin) (\c -> c /= end)
        |> Parser.map (\data -> MarkedText mark (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin + 1 })


codeParser : Int -> Parser Context Problem Token
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin })


symbolParser : Int -> Char -> Parser Context Problem Token
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin })
