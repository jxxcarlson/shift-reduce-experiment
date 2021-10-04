module Markup.L1 exposing (recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr(..))
import Markup.Common exposing (Step(..))
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal =
    identity


reduce : State -> State
reduce state =
    case state.stack of
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state

        (Left (Token.Verbatim name str loc)) :: [] ->
            reduceAux (AST.Verbatim name str loc) [] state

        (Left (Symbol "]" loc3)) :: (Left (Token.Text str loc2)) :: (Left (Symbol "[" loc1)) :: rest ->
            reduceAux (makeGExpr { begin = loc1.begin, end = loc3.end } str) rest state

        (Left (Symbol "]" loc4)) :: (Right expr) :: (Left (Token.Text name loc2)) :: (Left (Symbol "[" loc1)) :: rest ->
            reduceAux (makeGExpr2 { begin = loc1.begin, end = loc4.end } name expr) rest state

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


makeGExpr2 : Token.Loc -> String -> Expr -> Expr
makeGExpr2 loc name expr =
    Expr (String.trim name) [ expr ] loc


makeGExpr : Token.Loc -> String -> Expr
makeGExpr loc str =
    let
        words =
            String.words str

        prefix =
            List.head words |> Maybe.withDefault "empty"
    in
    -- TODO: not a good solution for loc
    Expr prefix (List.map (AST.Text >> (\t -> t loc)) (List.drop 1 words)) loc


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
