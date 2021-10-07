module Markup.MiniLaTeX exposing (recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr)
import Markup.Common exposing (Step(..))
import Markup.Debugger exposing (debug1)
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal : State -> State
reduceFinal state =
    case state.stack of
        (Right (AST.Expr name args loc)) :: [] ->
            { state | committed = AST.Expr (transformMacroNames name) (List.reverse args) loc :: state.committed, stack = [] } |> debug1 "FINAL RULE 1"

        (Left (FunctionName name loc)) :: [] ->
            { state | committed = AST.Expr (transformMacroNames name) [] loc :: state.committed, stack = [] } |> debug1 "FINAL RULE 2"

        _ ->
            state |> debug1 "FINAL RULE 2"


{-|

    Using patterns of the form a :: b :: c ... :: [ ] instead of a :: b :: c ... :: rest makes
    the reduction process greedy.

-}
reduce : State -> State
reduce state =
    case state.stack of
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state |> debug1 "RULE 1"

        (Left (Token.Symbol "}" loc4)) :: (Left (Token.Text arg loc3)) :: (Left (Token.Symbol "{" _)) :: (Left (Token.FunctionName name loc1)) :: rest ->
            { state | stack = Right (AST.Expr (transformMacroNames name) [ AST.Text arg loc3 ] { begin = loc1.begin, end = loc4.end }) :: rest } |> debug1 "RULE 2"

        (Left (Token.Symbol "}" loc4)) :: (Left (Token.Text arg loc3)) :: (Left (Token.Symbol "{" _)) :: (Right (AST.Expr name args loc1)) :: rest ->
            { state | stack = Right (AST.Expr (transformMacroNames name) (AST.Text arg loc3 :: args) { begin = loc1.begin, end = loc4.end }) :: rest } |> debug1 "RULE 3"

        (Left (Token.Text str loc2)) :: (Right (AST.Expr name args loc1)) :: rest ->
            { state | committed = AST.Text str loc2 :: AST.Expr (transformMacroNames name) (List.reverse args) loc1 :: state.committed, stack = rest } |> debug1 "RULE 4"

        (Left (Token.Symbol "}" loc4)) :: (Right (AST.Expr exprName args loc3)) :: (Left (Token.Symbol "{" _)) :: (Left (Token.FunctionName fName loc1)) :: rest ->
            { state | committed = AST.Expr fName [ AST.Expr (transformMacroNames exprName) args loc3 ] { begin = loc1.begin, end = loc4.end } :: state.committed, stack = rest } |> debug1 "RULE 5"

        (Left (Token.Verbatim label content loc)) :: [] ->
            reduceAux (AST.Verbatim label content loc) [] state |> debug1 "RULE 6"

        _ ->
            state


transformMacroNames : String -> String
transformMacroNames str =
    case str of
        "section" ->
            "heading2"

        "subsection" ->
            "heading3"

        "susubsection" ->
            "heading4"

        "subheading" ->
            "heading5"

        _ ->
            str


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux expr rest state =
    if rest == [] then
        { state | stack = [], committed = expr :: state.committed }

    else
        { state | stack = Right expr :: rest }


recoverFromError : State -> Step State State
recoverFromError state =
    -- Use this when the loop is about to exit but the stack is non-empty.
    -- Look for error patterns on the top of the stack.
    -- If one is found, modify the stack and push an error message onto state.committed; then loop
    -- If no pattern is found, make a best effort: push (Left (Symbol "]")) onto the stack,
    -- push an error message onto state.committed, then exit as usual: apply function reduce
    -- to the state and reverse state.committed.
    case state.stack of
        (Left (Token.Text _ loc1)) :: (Left (Symbol "[" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , committed = AST.Text "I corrected an unmatched '[' in the following expression: " Token.dummyLoc :: state.committed
                }

        (Left (Symbol "[" loc1)) :: (Left (Token.Text _ _)) :: (Left (Symbol "[" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , scanPointer = loc1.begin
                    , committed = AST.Text "I corrected an unmatched '[' in the following expression: " Token.dummyLoc :: state.committed
                }

        _ ->
            let
                position =
                    state.stack |> stackBottom |> Maybe.andThen scanPointerOfItem |> Maybe.withDefault state.scanPointer

                errorText =
                    String.dropLeft position state.sourceText

                errorMessage =
                    "Error! I added a bracket after this: " ++ errorText
            in
            Done
                ({ state
                    | stack = Left (Symbol "]" { begin = state.scanPointer, end = state.scanPointer + 1 }) :: state.stack
                    , committed = AST.Text errorMessage Token.dummyLoc :: state.committed
                 }
                    |> reduce
                    |> (\st -> { st | committed = List.reverse st.committed })
                )


stackBottom : List (Either Token Expr) -> Maybe (Either Token Expr)
stackBottom stack =
    List.head (List.reverse stack)


scanPointerOfItem : Either Token Expr -> Maybe Int
scanPointerOfItem item =
    case item of
        Left token ->
            Just (Token.startPositionOf token)

        Right _ ->
            Nothing
