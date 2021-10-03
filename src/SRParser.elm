module SRParser exposing (run)

import AST exposing (Expr(..))
import Common exposing (Step(..), loop)
import Debugger exposing (..)
import Either exposing (Either(..))
import L1
import MiniLaTeX
import State exposing (State)
import Token exposing (Token(..))
import Tokenizer exposing (Lang(..))



{-
   https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262/3
   https://www.cocolab.com/products/cocktail/doc.pdf/ell.pdf
   https://github.com/Janiczek/elm-grammar/tree/master/src
   https://guide.elm-lang.org/appendix/types_as_sets.html
   https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
-}


{-|

    Run the parser on some input, returning a value of type state.
    The stack in the final state should be empty

    > SRParser.run "foo [i [j ABC]]"
    { committed = [GText ("foo "),GExpr "i" [GExpr "j" [GText "ABC"]]], end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }

-}
run : Lang -> String -> State
run lang input =
    loop (init input) (nextState lang) |> debug3 "FINAL STATE"


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

    NOTE: the

-}
nextState : Lang -> State -> Step State State
nextState lang state_ =
    let
        count =
            state_.count + 1

        state =
            reduce lang ({ state_ | count = count } |> debug2 ("STATE (" ++ String.fromInt count ++ ")"))
    in
    if state.scanPointer >= state.end then
        -- Exit
        if state.stack == [] then
            Done (state |> (\st -> { st | committed = List.reverse st.committed }))

        else
            let
                newState =
                    reduceFinal lang state
            in
            if newState.stack == [] then
                Done (newState |> (\st -> { st | committed = List.reverse st.committed }))

            else
                -- Or recover from error
                recoverFromError lang state |> debug2 "ReduceFinal (2)"

    else
        -- Grab a new token from the source text
        case Tokenizer.get lang state.scanPointer (String.dropLeft state.scanPointer state.sourceText) of
            Err _ ->
                -- Oops, exit
                Done state

            Ok newToken ->
                -- Process the token: reduce the stack, then shift the token onto it.
                Loop (shift newToken (reduce lang state))


reduceFinal : Lang -> State -> State
reduceFinal lang =
    case lang of
        L1 ->
            L1.reduceFinal

        MiniLaTeX ->
            MiniLaTeX.reduceFinal


recoverFromError : Lang -> State -> Step State State
recoverFromError lang state =
    case lang of
        L1 ->
            L1.recoverFromError state

        MiniLaTeX ->
            MiniLaTeX.recoverFromError state


{-|

    Shift the new token onto the stack and advance the scan pointer

-}
shift : Token -> State -> State
shift token state =
    { state | scanPointer = state.scanPointer + Token.length token, stack = Either.Left token :: state.stack }


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
