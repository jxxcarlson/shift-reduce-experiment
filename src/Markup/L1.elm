module Markup.L1 exposing (makeLoc, recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import List.Extra
import Markup.AST as AST exposing (Expr(..))
import Markup.Common exposing (Step(..))
import Markup.Stack as Stack exposing (Stack)
import Markup.State exposing (State)
import Markup.Token as Token exposing (Token(..))


reduceFinal state =
    state


reduce : State -> State
reduce state =
    case state.stack of
        -- One term pattern:
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state

        -- One term pattern to handle verbatim text:
        (Left (Token.Verbatim name str loc)) :: rest ->
            if name == "code" then
                if String.left 2 str == "`!" then
                    reduceAux (AST.Text (String.dropLeft 2 (String.dropRight 1 str)) loc) rest state

                else
                    reduceAux (AST.Verbatim name (String.dropLeft 1 (String.dropRight 1 str)) loc) rest state

            else
                reduceAux (AST.Verbatim name str loc) rest state

        -- Two term pattern:
        (Left (Token.Text str loc)) :: (Right expr) :: [] ->
            { state | stack = [], committed = AST.Text str loc :: expr :: state.committed }

        -- Three term pattern for "ordinary" text like "[1]"  :
        (Left (Symbol "]" _)) :: (Left (Token.Text str loc2)) :: (Left (Symbol "[" _)) :: rest ->
            -- { state | stack = [], committed = AST.Text ("[" ++ str ++ "]") loc2 :: state.committed }
            reduceAux (AST.Text ("[" ++ str ++ "]") loc2) rest state

        -- Three term pattern:
        (Left (Symbol "]" _)) :: (Left (Token.Text str loc2)) :: (Left (FunctionName fragment loc1)) :: rest ->
            reduceAux (Expr (normalizeFragment fragment) [ AST.Text str loc2 ] (makeLoc loc1 loc2)) rest state

        -- Three term pattern:
        (Left (Symbol "]" loc3)) :: (Right expr) :: (Left (FunctionName fragment loc1)) :: rest ->
            reduceAux (Expr (normalizeFragment fragment) [ expr ] (makeLoc loc1 loc3)) rest state

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
                            Right (Expr (normalizeFragment name) (List.reverse exprList) loc) :: List.drop (n + 1) rest

                        _ ->
                            stack

        _ ->
            stack


normalizeFragment : String -> String
normalizeFragment str =
    str |> String.dropLeft 1 |> String.trimRight |> transform


makeLoc : Token.Loc -> Token.Loc -> Token.Loc
makeLoc loc1 loc2 =
    { begin = loc1.begin, end = loc2.end }


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux expr rest state =
    if rest == [] then
        { state | stack = [], committed = normalizeExpr expr :: state.committed }

    else
        { state | stack = Right (normalizeExpr expr) :: rest }


normalizeExpr : Expr -> Expr
normalizeExpr expr =
    case expr of
        Expr "link" exprList loc ->
            Expr "link" (AST.args2M exprList) loc

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
