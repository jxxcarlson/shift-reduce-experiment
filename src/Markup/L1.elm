module Markup.L1 exposing (makeLoc, recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr(..))
import Markup.Common exposing (Step(..))
import Markup.Stack
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal =
    identity


reduce : State -> State
reduce state =
    case state.stack of
        -- One term pattern:
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state

        -- One term pattern:
        (Left (Token.Verbatim name str loc)) :: [] ->
            reduceAux (AST.Verbatim name str loc) [] state

        -- Three term pattern:
        (Left (Symbol "]" loc3)) :: (Left (Token.Text str _)) :: (Left (Symbol "[" loc1)) :: rest ->
            reduceAux (makeExpr1 (makeLoc loc1 loc3) str) rest state

        -- Four term pattern:
        (Left (Symbol "]" loc4)) :: (Right expr) :: (Left (Token.Text name _)) :: (Left (Symbol "[" loc1)) :: rest ->
            reduceAux (makeExpr2 (makeLoc loc1 loc4) (transform name) (Debug.log "REDUCE EXXPR" expr)) rest state

        _ ->
            { state | stack = Markup.Stack.reduce1 state.stack }


makeLoc : Token.Loc -> Token.Loc -> Token.Loc
makeLoc loc1 loc2 =
    { begin = loc1.begin, end = loc2.end }


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux expr rest state =
    if rest == [] then
        { state | stack = [], committed = transformExpr expr :: state.committed }

    else
        { state | stack = Right expr :: rest }


transformExpr : Expr -> Expr
transformExpr expr =
    case expr of
        Expr name exprList loc ->
            Expr (transform name) (Debug.log "EXXPR (T)" exprList) loc

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


transform : String -> String
transform str =
    let
        _ =
            Debug.log "TRANSFORM (IN)" str
    in
    (case str of
        "i" ->
            "italic"

        "b" ->
            "strong"

        "h1" ->
            "heading1"

        "h2" ->
            "heading2"

        "h3" ->
            "heading3"

        "h4" ->
            "heading4"

        _ ->
            str
    )
        |> Debug.log "TRANSFORM (OUT)"


makeExpr2 : Token.Loc -> String -> Expr -> Expr
makeExpr2 loc name expr =
    Expr (String.trim (transform name)) [ expr ] loc


makeExpr1 : Token.Loc -> String -> Expr
makeExpr1 loc str =
    let
        words =
            String.words str

        prefix =
            List.head words |> Maybe.withDefault "empty"
    in
    -- TODO: not a good solution for loc
    -- Expr prefix (List.map (AST.Text >> (\t -> t loc)) (List.drop 1 words)) loc
    Expr prefix [ AST.Text (List.drop 1 words |> String.join " ") loc ] loc


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
