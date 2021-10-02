module Tokenizer exposing
    ( Token(..)
    , content
    , get
    , length
    )

import Error exposing (..)
import Parser.Advanced as Parser exposing (Parser)
import ParserTools


type Token
    = Text String Loc
    | Math String Loc
    | Code String Loc
    | Symbol String Loc


type alias Loc =
    { begin : Int, end : Int }


content : Token -> String
content token =
    case token of
        Text str _ ->
            str

        Math str _ ->
            str

        Code str _ ->
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
        |> Parser.map (\data -> Math data.content { begin = start, end = start + data.end - data.begin })


codeParser : Int -> Parser Context Problem Token
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Code data.content { begin = start, end = start + data.end - data.begin })



--spacesParser : Parser Context Problem Token
--spacesParser =
--    Parser.spaces |> Parser.map (\_ -> WS " ")


symbolParser : Int -> Char -> Parser Context Problem Token
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\c -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin })
