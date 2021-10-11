module Markup.Tokenizer exposing (get)

import Markup.Debugger exposing (..)
import Markup.Error exposing (..)
import Markup.Lang exposing (Lang(..))
import Markup.ParserTools as ParserTools
import Markup.Token exposing (Token(..))
import Parser.Advanced as Parser exposing (Parser)


{-|

    NOTES. In the computation of the end field of the Meta component of a Token,
    one must use the code `end = start + data.end - data.begin  - 1`.  The
    `-1` is because the data.end comes from the position of the scanPointer,
    which is at this juncture pointing one character beyond the string chomped.

-}
get : Lang -> Int -> String -> Result (List (Parser.DeadEnd Context Problem)) Token
get lang start input =
    Parser.run (tokenParser lang start) input |> debug2 "Tokenizer.get"


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
            Parser.oneOf
                [ textParser lang start
                , mathParser start
                , codeParser start
                , l1FunctionNameParser start
                , symbolParser start ']'
                ]

        MiniLaTeX ->
            Parser.oneOf
                [ textParser lang start
                , mathParser start
                , macroParser start
                , symbolParser start '{'
                , symbolParser start '}'
                ]

        Markdown ->
            Parser.oneOf
                [ markedTextParser start "strong" '*' '*'
                , markedTextParser start "italic" '_' '_'
                , markedTextParser start "code" '`' '`'
                , markedTextParser start "math" '$' '$'
                , markedTextParser start "arg" '(' ')'
                , markedTextParser start "annotation" '[' ']'
                , markedTextParser start "image" '!' ']'
                , textParser lang start
                ]


textParser : Lang -> Int -> Parser Context Problem Token
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


macroParser : Int -> Parser Context Problem Token
macroParser start =
    ParserTools.text (\c -> c == '\\') (\c -> c /= '{')
        |> Parser.map (\data -> FunctionName (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin - 1 })


mathParser : Int -> Parser Context Problem Token
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Verbatim "math" (dropFirstAndLastCharacter data.content) { begin = start, end = start + data.end - data.begin - 1 })


dropFirstAndLastCharacter str =
    String.slice 1 (String.length str - 1) str


markedTextParser : Int -> String -> Char -> Char -> Parser Context Problem Token
markedTextParser start mark begin end =
    ParserTools.text (\c -> c == begin) (\c -> c /= end)
        |> Parser.map (\data -> MarkedText mark (dropLeft mark data.content) { begin = start, end = start + data.end - data.begin })


dropLeft : String -> String -> String
dropLeft mark str =
    if mark == "image" then
        String.dropLeft 2 str

    else
        String.dropLeft 1 str


codeParser : Int -> Parser Context Problem Token
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin - 1 })


symbolParser : Int -> Char -> Parser Context Problem Token
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin - 1 })


l1FunctionNameParser : Int -> Parser Context Problem Token
l1FunctionNameParser start =
    ParserTools.textWithEndSymbol " " (\c -> c == '[') (\c -> c /= ' ')
        |> Parser.map (\data -> FunctionName data.content { begin = start, end = start + data.end - data.begin - 1 })
