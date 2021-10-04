module MarkupParser.Markdown exposing (recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import MarkupParser.AST as AST exposing (Expr(..))
import MarkupParser.Common as Common exposing (Step(..))
import MarkupParser.Debugger as Debugger exposing (debug1)
import MarkupParser.State as State exposing (State)
import MarkupParser.Token as Token exposing (Token(..))


reduceFinal : State -> State
reduceFinal state =
    case state.stack of
        (Right (AST.Expr name args)) :: [] ->
            { state | committed = AST.Expr name (List.reverse args) :: state.committed, stack = [] } |> debug1 "FINAL RULE 1"

        --
        --(Left (MarkedText "strong" str _)) :: [] ->
        --    { state | committed = Expr "strong" [ AST.Text str ] :: state.committed, stack = [] } |> debug1 "FINAL RULE 2"
        _ ->
            state |> debug1 "FINAL RULE LAST"


{-|

    Using patterns of the form a :: b :: c ... :: [ ] instead of a :: b :: c ... :: rest makes
    the reduction process greedy.

-}
reduce : State -> State
reduce state =
    case state.stack of
        (Left (Token.Text str _)) :: [] ->
            reduceAux (AST.Text str) [] state |> debug1 "RULE 1"

        (Left (MarkedText "strong" str _)) :: [] ->
            { state | committed = Expr "strong" [ AST.Text str ] :: state.committed, stack = [] }

        (Left (MarkedText "italic" str _)) :: [] ->
            { state | committed = Expr "italic" [ AST.Text str ] :: state.committed, stack = [] }

        (Left (MarkedText "code" str _)) :: [] ->
            { state | committed = Expr "code" [ AST.Text str ] :: state.committed, stack = [] }

        (Left (MarkedText "math" str _)) :: [] ->
            { state | committed = Expr "math" [ AST.Text str ] :: state.committed, stack = [] }

        (Left (MarkedText "arg" url _)) :: (Left (MarkedText "annotation" label _)) :: [] ->
            { state | committed = Expr "link" [ AST.Text label, AST.Text url ] :: state.committed, stack = [] }

        _ ->
            state


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
                    , committed = AST.Text "I corrected an unmatched '[' in the following expression: " :: state.committed
                }

        (Left (Symbol "[" loc1)) :: (Left (Token.Text _ _)) :: (Left (Symbol "[" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , scanPointer = loc1.begin
                    , committed = AST.Text "I corrected an unmatched '[' in the following expression: " :: state.committed
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
                    , committed = AST.Text errorMessage :: state.committed
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
