module Markup.Tokenizer exposing (get, linkParser, markedTextParser)

import Markup.Debugger exposing (..)
import Markup.Error exposing (..)
import Markup.L1 exposing (makeLoc)
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
get : Lang -> Int -> String -> Result (List (Parser.DeadEnd Context Problem)) Token
get lang start input =
    Parser.run (tokenParser lang start) input |> debug2 "Tokenizer.get"


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
                [ linkParser start
                , imageParser start
                , boldItalicTextParser start
                , italicBoldTextParser start
                , markedTextParser start "strong" '*' '*'
                , markedTextParser start "italic" '_' '_'
                , markedTextParser start "code" '`' '`'
                , markedTextParser start "math" '$' '$'
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


markedTextParser : Int -> String -> Char -> Char -> TokenParser
markedTextParser start mark begin end =
    ParserTools.text (\c -> c == begin) (\c -> c /= end)
        |> Parser.map (\data -> MarkedText mark (dropLeft mark data.content) { begin = start, end = start + data.end - data.begin })


linkParser : Int -> TokenParser
linkParser start =
    Parser.succeed (\begin annotation arg end -> AnnotatedText "link" annotation.content arg.content { begin = start + begin, end = start + end })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "[" (ExpectingSymbol "["))
        |= ParserTools.text (\c -> c /= '[') (\c -> c /= ']')
        |. Parser.symbol (Parser.Token "]" (ExpectingSymbol "]"))
        |. Parser.symbol (Parser.Token "(" (ExpectingSymbol "("))
        |= ParserTools.text (\c -> c /= '(') (\c -> c /= ')')
        |. Parser.symbol (Parser.Token ")" (ExpectingSymbol ")"))
        |= Parser.getOffset


imageParser : Int -> TokenParser
imageParser start =
    Parser.succeed (\begin annotation arg end -> AnnotatedText "image" annotation.content arg.content { begin = start + begin, end = start + end })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "![" (ExpectingSymbol "!["))
        |= ParserTools.text (\c -> c /= ']') (\c -> c /= ']')
        |. Parser.symbol (Parser.Token "]" (ExpectingSymbol "]"))
        |. Parser.symbol (Parser.Token "(" (ExpectingSymbol "("))
        |= ParserTools.text (\c -> c /= '(') (\c -> c /= ')')
        |. Parser.symbol (Parser.Token ")" (ExpectingSymbol ")"))
        |= Parser.getOffset


boldItalicTextParser : Int -> TokenParser
boldItalicTextParser start =
    Parser.succeed (\begin data end -> MarkedText "boldItalic" data.content { begin = start + begin, end = start + end })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "*_" (ExpectingSymbol "*_"))
        |= ParserTools.text (\c -> not (List.member c [ '*', '_' ])) (\c -> not (List.member c [ '*', '_' ]))
        |. Parser.symbol (Parser.Token "_*" (ExpectingSymbol "_*"))
        |= Parser.getOffset


italicBoldTextParser : Int -> TokenParser
italicBoldTextParser start =
    Parser.succeed (\begin data end -> MarkedText "boldItalic" data.content { begin = start + begin, end = start + end })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "_*" (ExpectingSymbol "_*"))
        |= ParserTools.text (\c -> not (List.member c [ '*', '_' ])) (\c -> not (List.member c [ '*', '_' ]))
        |. Parser.symbol (Parser.Token "*_" (ExpectingSymbol "*_"))
        |= Parser.getOffset


dropLeft : String -> String -> String
dropLeft mark str =
    if mark == "image" then
        String.dropLeft 2 str

    else
        String.dropLeft 1 str


codeParser : Int -> TokenParser
codeParser start =
    ParserTools.textWithEndSymbol "`" (\c -> c == '`') (\c -> c /= '`')
        |> Parser.map (\data -> Verbatim "code" data.content { begin = start, end = start + data.end - data.begin - 1 })


symbolParser : Int -> Char -> TokenParser
symbolParser start sym =
    ParserTools.text (\c -> c == sym) (\_ -> False)
        |> Parser.map (\data -> Symbol data.content { begin = start, end = start + data.end - data.begin - 1 })


l1FunctionNameParser : Int -> TokenParser
l1FunctionNameParser start =
    ParserTools.textWithEndSymbol " " (\c -> c == '[') (\c -> c /= ' ')
        |> Parser.map (\data -> FunctionName data.content { begin = start, end = start + data.end - data.begin - 1 })
