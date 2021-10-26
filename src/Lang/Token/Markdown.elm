module Lang.Token.Markdown exposing (specialParser, tokenParser)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Lang.Token.Common as Common
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type alias TokenParser =
    Parser Context Problem Token


tokenParser start =
    Parser.oneOf
        [
        imageParser start
        , Common.symbolParser start '['
        , Common.symbolParser start ']'
        , Common.symbolParser start '('
        , Common.symbolParser start ')'
        , markedTextParser start "strong" '*' '*'
        , markedTextParser start "italic" '_' '_'
        , markedTextParser start "code" '`' '`'
        , markedTextParser start "math" '$' '$'
        , Common.textParser Markdown start
        ]




markedTextParser : Int -> String -> Char -> Char -> TokenParser
markedTextParser start mark begin end =
    ParserTools.text (\c -> c == begin) (\c -> c /= end)
        |> Parser.map (\data -> MarkedText mark (dropLeft mark data.content) { begin = start, end = start + data.end - data.begin })

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




specialParser : Int -> TokenParser
specialParser start =
    Parser.succeed (\begin name argString end -> Special name.content argString.content { begin = start + begin, end = start + end })
        |= Parser.getOffset
        |. Parser.symbol (Parser.Token "@@ " (ExpectingSymbol "@"))
        |= ParserTools.text (\c -> c /= '[') (\c -> c /= '[')
        |. Parser.symbol (Parser.Token "[" (ExpectingSymbol "["))
        |= ParserTools.text (\c -> c /= ']') (\c -> c /= ']')
        |. Parser.symbol (Parser.Token "]" (ExpectingSymbol "]"))
        |= Parser.getOffset



dropLeft : String -> String -> String
dropLeft mark str =
    if mark == "image" then
        String.dropLeft 2 str

    else
        String.dropLeft 1 str
