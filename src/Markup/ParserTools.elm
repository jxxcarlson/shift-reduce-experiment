module Markup.ParserTools exposing
    ( Step(..)
    , StringData
    , text
    , textWithEndSymbol
    )

import Markup.Error exposing (Context, Problem(..))
import Parser.Advanced as Parser exposing ((|.), (|=))


type alias Parser a =
    Parser.Parser Context Problem a


type alias StringData =
    { begin : Int, end : Int, content : String }


{-| Get the longest string
whose first character satisfies `prefix` and whose remaining
characters satisfy `continue`. ParserTests:

    line =
        textPS (\c -> Char.isAlpha) [ '\n' ]

recognizes lines that start with an alphabetic character.

-}
text : (Char -> Bool) -> (Char -> Bool) -> Parser StringData
text prefix continue =
    Parser.succeed (\start finish content -> { begin = start, end = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefix c) ExpectingPrefix
        |. Parser.chompWhile (\c -> continue c)
        |= Parser.getOffset
        |= Parser.getSource


textWithEndSymbol : String -> (Char -> Bool) -> (Char -> Bool) -> Parser StringData
textWithEndSymbol symb prefix continue =
    Parser.succeed (\start finish content -> { begin = start, end = finish, content = String.slice start finish content })
        |= Parser.getOffset
        |. Parser.chompIf (\c -> prefix c) ExpectingPrefix
        |. Parser.chompWhile (\c -> continue c)
        |. Parser.symbol (Parser.Token symb (ExpectingSymbol symb))
        -- TODO: replace with real "Expecting"
        |= Parser.getOffset
        |= Parser.getSource



-- LOOP


type Step state a
    = Loop state
