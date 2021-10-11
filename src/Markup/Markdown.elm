module Markup.Markdown exposing (normalizeExpr, recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr(..))
import Markup.Common exposing (Step(..))
import Markup.Debugger exposing (debug1)
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal : State -> State
reduceFinal state =
    case state.stack of
        (Right (AST.Expr name args loc)) :: [] ->
            { state | committed = AST.Expr name (List.reverse args) loc :: state.committed, stack = [] } |> debug1 "FINAL RULE 1"

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
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state |> debug1 "RULE 1"

        (Left (MarkedText "boldItalic" str loc)) :: [] ->
            reduceAux (Expr "boldItalic" [ AST.Text str loc ] loc) [] state

        (Left (MarkedText "strong" str loc)) :: [] ->
            { state | committed = Expr "strong" [ AST.Text str loc ] loc :: state.committed, stack = [] }

        (Left (MarkedText "italic" str loc)) :: [] ->
            { state | committed = Expr "italic" [ AST.Text str loc ] loc :: state.committed, stack = [] }

        (Left (MarkedText "code" str loc)) :: [] ->
            { state | committed = AST.Verbatim "code" str loc :: state.committed, stack = [] }

        (Left (MarkedText "math" str loc)) :: [] ->
            { state | committed = AST.Verbatim "math" str loc :: state.committed, stack = [] }

        (Left (AnnotatedText "image" label value loc)) :: [] ->
            { state | committed = Expr "image" [ AST.Text value loc, AST.Text label loc ] loc :: state.committed, stack = [] }

        (Left (AnnotatedText "link" label value loc)) :: [] ->
            { state | committed = Expr "link" [ AST.Text label loc, AST.Text value loc ] loc :: state.committed, stack = [] }

        --(Left (MarkedText "arg" url loc2)) :: (Left (MarkedText "annotation" label loc1)) :: [] ->
        --    { state | committed = Expr "link" [ AST.Text label loc1, AST.Text url loc2 ] { begin = loc1.begin, end = loc2.end } :: state.committed, stack = [] }
        --
        --(Left (MarkedText "arg" url loc2)) :: (Left (MarkedText "image" label loc1)) :: [] ->
        --    { state | committed = normalizeExpr (Expr "image" [ AST.Text label loc1, AST.Text url loc2 ] { begin = loc1.begin, end = loc2.end }) :: state.committed, stack = [] }
        _ ->
            state


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux expr rest state =
    if rest == [] then
        { state | stack = [], committed = normalizeExpr expr :: state.committed }

    else
        { state | stack = Right (normalizeExpr expr) :: rest }


normalizeExpr : Expr -> Expr
normalizeExpr expr =
    case expr of
        Expr "image" exprList loc ->
            Expr "image" (List.drop 1 exprList) loc

        _ ->
            expr


recoverFromError : State -> Step State State
recoverFromError state =
    -- Use this when the loop is about to exit but the stack is non-empty.
    -- Look for error patterns on the top of the stack.
    -- If one is found, modify the stack and push an error message onto state.committed; then loop
    -- If no pattern is found, make a best effort: push (Left (Symbol "]")) onto the stack,
    -- push an error message onto state.committed, then exit as usual: apply function reduce
    -- to the state and reverse state.committed.
    case state.stack of
        (Left (Token.Text _ _)) :: (Left (Symbol "[" loc1)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , committed = AST.Text "I corrected an unmatched '[' in the following expression: " Token.dummyLoc :: state.committed
                }

        (Left (Symbol "[" _)) :: (Left (Token.Text _ _)) :: (Left (Symbol "[" loc1)) :: _ ->
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
