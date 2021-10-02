module Tokenizer exposing
    ( Token(..)
    , content
    , get
    , length
    , run
    )

import Error exposing (..)
import Parser.Advanced as Parser exposing (Parser)
import ParserTools


type Token
    = Text String
    | Math String
    | Code String
    | Symbol String


content : Token -> String
content token =
    case token of
        Text str ->
            str

        Math str ->
            str

        Code str ->
            str

        Symbol str ->
            str


length : Token -> Int
length token =
    String.length (content token)


get : String -> Result (List (Parser.DeadEnd Context Problem)) Token
get input =
    Parser.run tokenParser input


run : String -> Result (List (Parser.DeadEnd Context Problem)) (List Token)
run input =
    Parser.run (ParserTools.many tokenParser) input


languageChars =
    [ '[', ']', '`', '$' ]


{-|

    > Tokenizer.run "Test: [i [j foo bar]]"
      Ok [Text ("Test: "),Symbol "[",Text ("i "),Symbol "[",Text ("j foo bar"),Symbol "]",Symbol "]"]

-}
tokenParser =
    Parser.oneOf [ textParser, mathParser, codeParser, symbolParser "[", symbolParser "]" ]


textParser : Parser Context Problem Token
textParser =
    ParserTools.text (\c -> not <| List.member c languageChars) (\c -> not <| List.member c languageChars)
        |> Parser.map (.content >> Text)


mathParser : Parser Context Problem Token
mathParser =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (.content >> Math)


codeParser : Parser Context Problem Token
codeParser =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (.content >> Code)



--spacesParser : Parser Context Problem Token
--spacesParser =
--    Parser.spaces |> Parser.map (\_ -> WS " ")


symbolParser : String -> Parser Context Problem Token
symbolParser sym =
    Parser.symbol (Parser.Token sym (ExpectingSymbol sym)) |> Parser.map (\_ -> Symbol sym)
