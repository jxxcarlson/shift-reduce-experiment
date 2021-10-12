module Expression.Tokenizer exposing (get)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Lang.Token.L1 as L1
import Lang.Token.Markdown as Markdown
import Lang.Token.MiniLaTeX as MiniLaTeX
import Markup.ParserTools as ParserTools
import Parser.Advanced as Parser exposing (Parser)


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
            TokenError (errorList |> Debug.log "ERROR LIST") { begin = start, end = start + 1 }



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
            L1.tokenParser start

        MiniLaTeX ->
            MiniLaTeX.tokenParser start

        Markdown ->
            Markdown.tokenParser start
