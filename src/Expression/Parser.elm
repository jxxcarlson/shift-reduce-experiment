module Expression.Parser exposing (parseExpr, parseToBlock, run)

import Block.Block exposing (Block)
import Block.BlockTools
import Either
import Expression.AST as AST exposing (Expr)
import Expression.Error exposing (ErrorData, Problem(..))
import Expression.State exposing (State)
import Expression.Token as Token exposing (Token(..), dummyLoc)
import Expression.Tokenizer as Tokenizer
import Lang.Lang exposing (Lang(..))
import Lang.Reduce.L1 as L1
import Lang.Reduce.Markdown as Markdown
import Lang.Reduce.MiniLaTeX as MiniLaTeX
import List.Extra
import Markup.Common exposing (Step(..), loop)
import Markup.Debugger exposing (..)


parseExpr : Lang -> String -> List AST.Expr
parseExpr lang str =
    run lang str |> .committed



{-
   https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262/3
   https://www.cocolab.com/products/cocktail/doc.pdf/ell.pdf
   https://github.com/Janiczek/elm-grammar/tree/master/src
   https://guide.elm-lang.org/appendix/types_as_sets.html
   https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
-}


parseToBlock : Lang -> String -> Int -> String -> Block
parseToBlock lang id firstLine str =
    Block.BlockTools.make id firstLine str |> Block.BlockTools.map (parseExpr lang)


{-|

    Run the parser on some input, returning a value of type state.
    The stack in the final state should be empty

    > Markup.run "foo [i [j ABC]]"
    { committed = [GText ("foo "),GExpr "i" [GExpr "j" [GText "ABC"]]], end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }

-}
run : Lang -> String -> State
run lang input =
    loop (init input) (nextState lang) |> debug2 "FINAL STATE"


init : String -> State
init str =
    { sourceText = str
    , scanPointer = 0
    , end = String.length str
    , stack = []
    , committed = []
    , count = 0
    }


{-|

    If scanPointer == end, you are done.
    Otherwise, get a new token from the source text, reduce the stack,
    and shift the new token onto the stack.

    NOTES:

        - The reduce function is applied in two places: the top-level
          function nextState and in the Loop branch of processToken.

        - In addition, there is the function reduceFinal, which is applied
          in the first branch of auxiliary function nextState_

        - Both reduce and reduceFinal call out to corresponding versions
          of these functions for the language being processed.  See folders
          L1, MiniLaTeX and Markdown

       - The dependency on language is via (1) the two reduce functions and
         (2) the tokenization function. In particular, there is no
         language dependency, other than the lang argument,
         in the main parser module (this module).

-}
nextState : Lang -> State -> Step State State
nextState lang state_ =
    { state_ | count = state_.count + 1 }
        -- |> debug2 ("STATE (" ++ String.fromInt (state_.count + 1) ++ ")")
        |> reduce lang
        |> nextState_ lang


nextState_ : Lang -> State -> Step State State
nextState_ lang state =
    if state.scanPointer >= state.end then
        finalize lang (reduceFinal lang state |> debug1 "reduceFinal (APPL)")

    else
        processToken lang state


finalize : Lang -> State -> Step State State
finalize lang state =
    if state.stack == [] then
        Done (state |> (\st -> { st | committed = List.reverse st.committed })) |> debug2 "ReduceFinal (1)"

    else
        recoverFromError lang state |> debug2 "ReduceFinal (2, recoverFromErrors)"


processToken : Lang -> State -> Step State State
processToken lang state =
    case Tokenizer.get lang state.scanPointer (String.dropLeft state.scanPointer state.sourceText) of
        TokenError errorData meta ->
            let
                ( row, col ) =
                    List.map (\item -> ( item.row, item.col )) errorData |> List.Extra.last |> Maybe.withDefault ( 1, 1 ) |> debug4 "(row, col)"

                firstLines =
                    List.take (row - 1) (String.lines (String.dropLeft state.scanPointer state.sourceText)) |> debug4 "first lines"

                lastLine =
                    List.drop (row - 1) (String.lines (String.dropLeft state.scanPointer state.sourceText)) |> String.join "" |> String.left col |> debug4 "last line"

                unprocessedText =
                    String.join "\n" firstLines ++ lastLine |> debug4 "unprocessedText"

                tokenLength =
                    String.length unprocessedText |> debug4 "tokenLength"
            in
            -- Oops, exit
            Loop { state | committed = errorValue state errorData :: state.committed, scanPointer = state.scanPointer + tokenLength + 1 }

        newToken ->
            Loop (shift newToken (reduce lang state))


errorValue : State -> ErrorData -> Expr
errorValue state errorData =
    let
        problems =
            List.map .problem errorData

        remaining =
            String.dropLeft state.scanPointer state.sourceText |> String.trimRight
    in
    case List.head problems of
        Just (ExpectingSymbol "$") ->
            AST.Verbatim "math" (remaining ++ "$") dummyLoc

        _ ->
            AST.Text ("Can't correct this text: " ++ remaining) dummyLoc


reduceFinal : Lang -> State -> State
reduceFinal lang =
    case lang of
        L1 ->
            L1.reduceFinal

        MiniLaTeX ->
            MiniLaTeX.reduceFinal

        Markdown ->
            Markdown.reduceFinal


recoverFromError : Lang -> State -> Step State State
recoverFromError lang state =
    case lang of
        L1 ->
            L1.recoverFromError state

        MiniLaTeX ->
            MiniLaTeX.recoverFromError state

        Markdown ->
            Markdown.recoverFromError state


{-|

    Shift the new token onto the stack and advance the scan pointer

-}
shift : Token -> State -> State
shift token state =
    -- It is essential to add 1 to the length of the token so that the scanpointer points to the character
    -- immediately after the current token in the source text.
    { state | scanPointer = state.scanPointer + Token.length token + 1, stack = Either.Left token :: state.stack }


{-|

    Function reduce matches patterns at the top of the stack, then from the given instance
    of that pattern creates a GExpr.  Let the stack be (a::b::..::rest).  If rest
    is empty, push the new GExpr onto state.committed.  If not, push (Right GExpr)
    onto rest.  The stack now reads (Right GExpr)::rest.

    Note that the stack has type List (Either Token GExpr).

    NOTE: The pattern -> action clauses below invert productions in the grammar and so
    one should be able to deduce them mechanically from the grammar.

-}
reduce : Lang -> State -> State
reduce lang state =
    case lang of
        L1 ->
            L1.reduce state

        MiniLaTeX ->
            MiniLaTeX.reduce state

        Markdown ->
            Markdown.reduce state
