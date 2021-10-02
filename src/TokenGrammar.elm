module TokenGrammar exposing (acceptCharsParser, rejectCharsParser, tokenGrammarParser)

import Error exposing (Context, Problem(..))
import Maybe.Extra
import Parser.Advanced as Parser
import ParserTools exposing (StringData)
import TokenPart exposing (TokenPart(..))


type alias Parser a =
    Parser.Parser Context Problem a


tokenGrammarParser : Parser (Maybe (List TokenPart))
tokenGrammarParser =
    ParserTools.many acceptOrRejectParser |> Parser.map Maybe.Extra.combine


acceptOrRejectParser : Parser (Maybe TokenPart)
acceptOrRejectParser =
    Parser.oneOf [ acceptCharsParser, rejectCharsParser ]


acceptCharsParser : Parser (Maybe TokenPart)
acceptCharsParser =
    ParserTools.text (\c -> not <| List.member c [ '<', '>' ])
        (\c -> not <| List.member c [ '<', '>' ])
        |> Parser.map makeAcceptChars


rejectCharsParser : Parser (Maybe TokenPart)
rejectCharsParser =
    ParserTools.textWithEndBeginAndEndChar '<' '>' (\c -> not <| List.member c [ '<', '>' ])
        |> Parser.map makeRejectChars


makeAcceptChars : StringData -> Maybe TokenPart
makeAcceptChars stringData =
    let
        chars =
            String.toList stringData.content
    in
    case chars of
        [] ->
            Nothing

        [ c ] ->
            Just (AcceptChar c)

        _ ->
            Just (AcceptCharList chars)


makeRejectChars : StringData -> Maybe TokenPart
makeRejectChars stringData =
    let
        chars =
            String.toList stringData.content
                |> List.drop 1
                |> List.reverse
                |> List.drop 1
    in
    case chars of
        [] ->
            Nothing

        [ c ] ->
            Just (RejectChar c)

        _ ->
            Just (RejectCharList chars)
