module Markup.L1 exposing (makeLoc, recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import List.Extra
import Markup.AST as AST exposing (Expr(..))
import Markup.Common exposing (Step(..))
import Markup.Debugger exposing (debug1, debug3, debug4)
import Markup.Simplify as Simplify
import Markup.Stack as Stack exposing (Stack)
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal state =
    let
        _ =
            debug4 "reduceFinal, STACK (IN)" (state.stack |> Simplify.stack)
    in
    case state.stack of
        [] ->
            state

        (Left (Token.Text str loc)) :: rest ->
            reduceFinal { state | committed = AST.Text str loc :: state.committed, stack = rest }

        (Right expr) :: rest ->
            reduceFinal { state | committed = expr :: state.committed, stack = rest }

        _ ->
            state


reduce : State -> State
reduce state =
    case state.stack of
        -- One term pattern:
        (Left (Token.Text str loc)) :: [] ->
            let
                _ =
                    debug3 "Pattern" "One term (1: USED)"
            in
            reduceAux (AST.Text str loc) [] state

        -- One term pattern (X?):
        --(Left (Token.Verbatim name str loc)) :: [] ->
        --    let
        --        _ =
        --            debug3 "Pattern" "One term (2)"
        --    in
        --    reduceAux (AST.Verbatim name str loc) [] state
        -- Three term pattern:
        (Left (Symbol "]" loc3)) :: (Left (Token.Text str loc2)) :: (Left (FunctionName fragment loc1)) :: rest ->
            let
                _ =
                    debug3 "Pattern" "Three term (1: USED)"
            in
            reduceAux (Expr (normalizeFragment fragment) [ AST.Text str loc2 ] (makeLoc loc1 loc2)) rest state

        -- Three term pattern:
        --(Left (Symbol "]" loc3)) :: (Left (Token.Text str _)) :: (Left (Symbol "[" loc1)) :: rest ->
        --    let
        --        _ =
        --            debug3 "Pattern" "Three term (2)"
        --    in
        --    reduceAux (makeExpr1 (makeLoc loc1 loc3) str) rest state
        (Left (Symbol "]" loc3)) :: (Right expr) :: (Left (FunctionName fragment loc1)) :: rest ->
            let
                _ =
                    debug3 "Pattern" "Three term (3: USED)"

                --  [ Left (SymbolST "]"), Right (ExprS "j " [ TextS "ABC" ]), Left (FunctionNameST "[i ") ]
            in
            reduceAux (Expr (normalizeFragment fragment) [ expr ] (makeLoc loc1 loc3)) rest state

        -- Four term pattern (X?):
        --(Left (Symbol "]" loc4)) :: (Right expr) :: (Left (Token.Text name _)) :: (Left (Symbol "[" loc1)) :: rest ->
        --    let
        --        _ =
        --            debug3 "Pattern" "Four term (X?)"
        --    in
        --    reduceAux (makeExpr2 (makeLoc loc1 loc4) (transform name) (Debug.log "REDUCE EXXPR" expr)) rest state
        -- Four term pattern:
        --(Left (Symbol "]" loc4)) :: (Right (Expr name [ AST.Text str loc3 ] loc3b)) :: (Left (Token.Text " " loc2)) :: (Left (FunctionName fragment loc1)) :: rest ->
        --    let
        --        _ =
        --            debug3 "Pattern" "Four term (Y?)"
        --    in
        --    reduceAux (Expr (normalizeFragment fragment) [ AST.Text str loc2 ] (makeLoc loc1 loc2)) rest state
        _ ->
            { state | stack = reduce2 state.stack }


reduce2 : Stack -> Stack
reduce2 stack =
    case stack of
        (Left (Token.Symbol "]" loc1)) :: rest ->
            let
                interior =
                    List.Extra.takeWhile (\item -> not (Stack.isFunctionName item)) rest

                n =
                    List.length interior
            in
            case ( List.Extra.getAt n rest, Stack.toExprList interior ) of
                ( Nothing, _ ) ->
                    stack

                ( _, Nothing ) ->
                    stack

                ( Just stackItem, Just exprList ) ->
                    case stackItem of
                        Left (Token.FunctionName name loc) ->
                            Right (Expr name exprList loc) :: List.drop (n + 1) rest |> debug4 "REDUCE 1b (ACTION)"

                        _ ->
                            stack

        _ ->
            stack


normalizeFragment : String -> String
normalizeFragment str =
    str |> String.dropLeft 1 |> String.trimRight


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
            Expr (transform name) exprList loc

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
    case str of
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
