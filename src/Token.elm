module Token exposing (makeParserFromGrammar, parserFromPartsList)

import Error exposing (Context, Problem(..))
import Parser.Advanced as Parser
import ParserTools exposing (StringData)
import TokenGrammar
import TokenPart exposing (TokenPart(..))


type alias Parser a =
    Parser.Parser Context Problem a


makeParserFromGrammar : String -> Maybe (Parser (List StringData))
makeParserFromGrammar grammarString =
    case Parser.run TokenGrammar.tokenGrammarParser grammarString of
        Err _ ->
            Nothing

        Ok tokenPartsList ->
            Maybe.map parserFromPartsList tokenPartsList


parserFromPartsList : List TokenPart -> Parser (List StringData)
parserFromPartsList tokenParts =
    ParserTools.sequence (List.map parserFromPart tokenParts)


parserFromPart : TokenPart -> Parser StringData
parserFromPart tokenPart =
    case tokenPart of
        AcceptChar c ->
            ParserTools.text (\c_ -> c_ == c) (\_ -> False)

        RejectCharList charList ->
            ParserTools.text (\c_ -> not (List.member c_ charList)) (\c_ -> not (List.member c_ charList))

        RejectChar c ->
            ParserTools.text (\c_ -> c_ == c) (\_ -> False)

        AcceptCharList charList ->
            ParserTools.text (\c_ -> not (List.member c_ charList)) (\c_ -> not (List.member c_ charList))
