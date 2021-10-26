module Expression.Tokenizer exposing (get, run)

import Expression.Error exposing (..)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Lang.Token.L1 as L1
import Lang.Token.Markdown as Markdown
import Lang.Token.MiniLaTeX as MiniLaTeX
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
            TokenError errorList { begin = start, end = start + 1 }


type alias State = { source : String, scanPointer : Int , sourceLength :Int,  tokens : List Token}

{-|
    >  run MiniLaTeX "\\foo{1}"
      [FunctionName "foo" { begin = 0, end = 3 },Symbol "{" { begin = 4, end = 4 },Text "1" { begin = 5, end = 5 },Symbol "}" { begin = 6, end = 6 }]
-}
run : Lang -> String -> List Token
run lang source =
    loop (init source) (nextStep lang)


init : String -> State
init source = {source = source, scanPointer = 0, sourceLength =  String.length source, tokens = []}

nextStep : Lang -> State -> Step State (List Token)
nextStep lang state =
    if state.scanPointer >= state.sourceLength then
       Done (List.reverse state.tokens)
    else
       let
           token = get lang state.scanPointer (String.dropLeft state.scanPointer state.source )
           newScanPointer = state.scanPointer + (Expression.Token.length token) + 1
       in
       Loop { state | tokens = token :: state.tokens, scanPointer = newScanPointer}

type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b



--  Err [{ col = 1, contextStack = [], problem = ExpectingSymbol "$", row = 2 }]
--|> debug2 "Tokenizer.get"


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
