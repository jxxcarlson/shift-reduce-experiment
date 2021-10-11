module Markup.Tokenizer exposing (get)

import Lang.L1
import Lang.Markdown
import Markup.Debugger exposing (..)
import Markup.Error exposing (..)
import Markup.Lang exposing (Lang(..))
import Markup.ParserTools as ParserTools
import Markup.Token exposing (Token(..))
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


{-|

    NOTES. In the computation of the end field of the Meta component of a Token,
    one must use the code `end = start + data.end - data.begin  - 1`.  The
    `-1` is because the data.end comes from the position of the scanPointer,
    which is at this juncture pointing one character beyond the string chomped.

-}
get : Lang -> Int -> String -> Token
get lang start input =
    case Parser.run (tokenParser lang start) input of
        Ok token ->
            token

        Err errorList ->
            TokenError errorList { begin = 0, end = 0 }



--  Err [{ col = 1, contextStack = [], problem = ExpectingSymbol "$", row = 2 }]
--|> debug2 "Tokenizer.get"


l1LanguageChars =
    [ '[', ']', '`', '$' ]


miniLaTeXLanguageChars =
    [ '{', '}', '\\', '$' ]


markdownLanguageChars =
    [ '*', '_', '`', '$', '#' ]


type alias TokenParser =
    Parser Context Problem Token


{-|

    > Tokenizer.run "Test: [i [j foo bar]]"
      Ok [Text ("Test: "),Symbol "[",Text ("i "),Symbol "[",Text ("j foo bar"),Symbol "]",Symbol "]"]

-}
tokenParser : Lang -> Int -> TokenParser
tokenParser lang start =
    case lang of
        L1 ->
            Parser.oneOf
                [ textParser lang start
                , mathParser start
                , Lang.L1.codeParser start
                , Lang.L1.functionNameParser start
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
                [ Lang.Markdown.linkParser start
                , Lang.Markdown.imageParser start
                , Lang.Markdown.boldItalicTextParser start
                , Lang.Markdown.italicBoldTextParser start
                , Lang.Markdown.markedTextParser start "strong" '*' '*'
                , Lang.Markdown.markedTextParser start "italic" '_' '_'
                , Lang.Markdown.markedTextParser start "code" '`' '`'
                , Lang.Markdown.markedTextParser start "math" '$' '$'
                , textParser lang start
                ]



--tokenListToToken : List Token -> Token
--tokenListToToken tokenList =
--    case tokenList of
--        (MarkedText "arg" arg loc2) :: (MarkedText "annotation" annotation loc1) :: [] ->
--            AnnotatedText annotation arg (makeLoc loc1 loc2)
--
--        _ ->
--            MarkedText "error" "tokenList is not of the form [annotation](arg)" { begin = 0, end = 0 }


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


macroParser : Int -> TokenParser
macroParser start =
    ParserTools.text (\c -> c == '\\') (\c -> c /= '{')
        |> Parser.map (\data -> FunctionName (String.dropLeft 1 data.content) { begin = start, end = start + data.end - data.begin - 1 })


mathParser : Int -> TokenParser
mathParser start =
    ParserTools.textWithEndSymbol "$" (\c -> c == '$') (\c -> c /= '$')
        |> Parser.map (\data -> Verbatim "math" (dropFirstAndLastCharacter data.content) { begin = start, end = start + data.end - data.begin - 1 })


dropFirstAndLastCharacter str =
    String.slice 1 (String.length str - 1) str


symbolParser : Int -> Char -> TokenParser
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin - 1 })
