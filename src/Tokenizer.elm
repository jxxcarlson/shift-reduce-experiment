module Tokenizer exposing
    ( content
    , get
    , length
    , startPositionOf
    )

import Error exposing (..)
import Parser.Advanced as Parser exposing (Parser)
import ParserTools
import Token exposing (Loc, Token(..))


startPositionOf : Token -> Int
startPositionOf token =
    case token of
        Text _ loc ->
            loc.begin

        Verbatim _ _ loc ->
            loc.begin

        Symbol _ loc ->
            loc.begin


content : Token -> String
content token =
    case token of
        Text str _ ->
            str

        Verbatim _ str _ ->
            str

        Symbol str _ ->
            str


length : Token -> Int
length token =
    String.length (content token)


get : Int -> String -> Result (List (Parser.DeadEnd Context Problem)) Token
get start input =
    Parser.run (tokenParser start) input


languageChars =
    [ '[', ']', '`', '$' ]


{-|

    > Tokenizer.run "Test: [i [j foo bar]]"
      Ok [Text ("Test: "),Symbol "[",Text ("i "),Symbol "[",Text ("j foo bar"),Symbol "]",Symbol "]"]

-}
tokenParser start =
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



--spacesParser : Parser Context Problem Token
--spacesParser =
--    Parser.spaces |> Parser.map (\_ -> WS " ")


symbolParser : Int -> Char -> Parser Context Problem Token
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin })
