module Lang.Token.Markdown exposing (specialParser, tokenParser)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Token.Common as Common exposing (TokenParser, TokenState(..))
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)


type alias TokenParser =
    Parser Context Problem Token


{-| tokenParser calls a specific parser depending on the
value of tokenState. tokenParserA does not recognize
parentheses, while tokenParserB does. The transition from
state TSA to TSB 0 is made when a left bracket is encountered.
See Expression.Parser.nextTokenState. While in state TSB,
k is incremented when a left parenthesis is encountered,
decremented when a right parenthesis is encountered.
When a white-space token is encountered in state TSB \_,
the state changes to TSA.

All of this for the tokenizer to set things up to recognize
"function call" in extended markdown of the form

    [!function-name](arg_1)...(arg_n)

-}
tokenParser : TokenState -> Int -> Parser Context Problem Token
tokenParser tokenState start =
    case tokenState of
        TSA ->
            tokenParserA start

        TSB _ ->
            tokenParserB start


tokenParserA start =
    Parser.oneOf
        [ imageParser start
        , Common.symbolParser start '['
        , Common.symbolParser start ']'
        , markedTextParser start "strong" '*' '*'
        , markedTextParser start "italic" '_' '_'
        , markedTextParser start "code" '`' '`'
        , markedTextParser start "math" '$' '$'
        , textParserA start
        ]


tokenParserB start =
    Parser.oneOf
        [ imageParser start
        , Common.symbolParser start '['
        , Common.symbolParser start ']'
        , Common.symbolParser start '('
        , Common.symbolParser start ')'
        , markedTextParser start "strong" '*' '*'
        , markedTextParser start "italic" '_' '_'
        , markedTextParser start "code" '`' '`'
        , markedTextParser start "math" '$' '$'
        , textParserB start
        ]


textParserA start =
    ParserTools.text (\c -> not <| List.member c markdownLanguageCharsA) (\c -> not <| List.member c markdownLanguageCharsA)
        |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin - 1 })


textParserB start =
    ParserTools.text (\c -> not <| List.member c markdownLanguageCharsB) (\c -> not <| List.member c markdownLanguageCharsB)
        |> Parser.map (\data -> Text data.content { begin = start, end = start + data.end - data.begin - 1 })


markdownLanguageCharsA =
    [ '*', '_', '`', '$', '#', '[', ']' ]


markdownLanguageCharsB =
    [ '*', '_', '`', '$', '#', '[', ']', '(', ')' ]


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
