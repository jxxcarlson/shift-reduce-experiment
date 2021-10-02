module Token exposing (TokenPart(..), parserFromPartsList)

import Error exposing (Context, Problem(..))
import Parser.Advanced as Parser
import ParserTools exposing (StringData)


type alias Parser a =
    Parser.Parser Context Problem a


type TokenPart
    = Char Char
    | Reject (List Char)


parserFromPartsList : List TokenPart -> Parser (List StringData)
parserFromPartsList tokenParts =
    sequence (List.map parserFromPart tokenParts)


parserFromPart : TokenPart -> Parser StringData
parserFromPart tokenPart =
    case tokenPart of
        Char c ->
            ParserTools.text (\c_ -> c_ == c) (\_ -> False)

        Reject charList ->
            ParserTools.text (\c_ -> not (List.member c_ charList)) (\c_ -> not (List.member c_ charList))


sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    Parser.loop { parsers = parsers, results = [] } sequenceAux


type alias State a =
    { parsers : List (Parser a), results : List a }


sequenceAux : State a -> Parser (Parser.Step (State a) (List a))
sequenceAux state =
    case List.head state.parsers of
        Nothing ->
            Parser.succeed () |> Parser.map (\_ -> Parser.Done (List.reverse state.results))

        Just parser ->
            parser |> Parser.map (\a -> Parser.Loop { state | results = a :: state.results, parsers = List.drop 1 state.parsers })
