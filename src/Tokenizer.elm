module Tokenizer exposing (Lang(..), get)

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


languageChars =
    [ '[', ']', '`', '$' ]


{-|

    > Tokenizer.run "Test: [i [j foo bar]]"
      Ok [Text ("Test: "),Symbol "[",Text ("i "),Symbol "[",Text ("j foo bar"),Symbol "]",Symbol "]"]

-}
tokenParser lang start =
    case lang of
        L1 ->
            Parser.oneOf [ textParser start, mathParser start, codeParser start, symbolParser start '[', symbolParser start ']' ]

        MiniLaTeX ->
            Parser.oneOf [ textParser start, mathParser start, codeParser start, symbolParser start '[', symbolParser start ']' ]


textParser : Int -> Parser Context Problem Token
textParser start =
    ParserTools.text (\c -> not <| List.member c languageChars) (\c -> not <| List.member c languageChars)
        |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin })


mathParser : Int -> Parser Context Problem Token
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Verbatim "math" data.content { begin = start, end = start + data.end - data.begin })


codeParser : Int -> Parser Context Problem Token
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin })


symbolParser : Int -> Char -> Parser Context Problem Token
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin })
