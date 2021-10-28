module Lang.Reduce.Markdown exposing (normalizeExpr, recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Expression.AST as AST exposing (Expr(..))
import Expression.State exposing (State)
import Expression.Token as Token exposing (Token(..))
import Markup.Common exposing (Step(..))
import Markup.Debugger exposing (debugGreen, debugNull, debugRed, debugYellow)


reduceFinal : State -> State
reduceFinal state =
    state |> debugYellow "reduceFinal  (IN) " |> reduce |> reduceFinal_ |> debugYellow "reduceFinal  (OUT)"


reduceFinal_ : State -> State
reduceFinal_ state =
    case state.stack of
        (Right (AST.Expr name args loc)) :: [] ->
            { state | committed = AST.Expr name (List.reverse args) loc :: state.committed, stack = [] } |> debugRed "FINAL RULE 1"

        (Left (Token.Text str loc1)) :: (Right (AST.Expr name args loc2)) :: rest ->
            let
                _ =
                    debugRed "reduceFinal, RULE 2, IN" state
            in
            reduceFinal { state | committed = AST.Text str loc1 :: AST.Expr name (List.reverse args) loc2 :: state.committed, stack = rest } |> debugYellow "FINAL RULE 2"

        (Left (Token.Text str loc)) :: rest ->
            let
                _ =
                    debugRed "reduceFinal, RULE 3, IN" state
            in
            reduceFinal { state | committed = AST.Text str loc :: state.committed, stack = rest } |> debugYellow "FINAL RULE 2"

        _ ->
            state |> debugRed "FINAL RULE 4 (No action)"


{-|

    Using patterns of the form a :: b :: c ... :: [ ] instead of a :: b :: c ... :: rest makes
    the reduction process greedy.

-}
reduce : State -> State
reduce state =
    case state.stack of
        -- commit lone text token
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state |> debugGreen "RED 6 (RULE 1)"

        -- commit text :: expression
        (Left (Token.Text str loc1)) :: (Right (AST.Expr name args loc2)) :: rest ->
            let
                _ =
                    debugRed "reduceFinal, RULE 2, IN" state
            in
            { state | committed = AST.Text str loc1 :: AST.Expr name (List.reverse args) loc2 :: state.committed, stack = rest } |> debugYellow "FINAL RULE 2"

        -- RULE R1: L S [, L  T fname, L S ] -> Right Expr fname []
        (Left (Token.Symbol "]" loc3)) :: (Left (Token.Text fName loc2)) :: (Left (Token.Symbol "[" loc1)) :: rest ->
            let
                expr =
                    if String.left 1 fName == "!" then
                        Right (Expr (String.trim <| String.dropLeft 1 fName) [] { begin = loc1.begin, end = loc3.end })

                    else
                        Right (Expr "link" [ AST.Text fName loc2 ] { begin = loc1.begin, end = loc3.end })
            in
            { state | stack = expr :: rest } |> debugGreen "RED 1"

        -- RULE R2: L S (, L  T arg, L S ) -> Right Arg [T arg]
        (Left (Token.Symbol ")" loc3)) :: (Left (Token.Text arg loc2)) :: (Left (Token.Symbol "(" loc1)) :: rest ->
            let
                expr =
                    Right (Arg [ AST.Text arg loc2 ] { begin = loc1.begin, end = loc3.end })
            in
            { state | stack = expr :: rest } |> debugGreen "RED 2"

        -- RULE R3: L S (, R Expr fname args, L S ) -> R Arg [Expr fname  args]
        (Left (Token.Symbol ")" loc3)) :: (Right expr_) :: (Left (Token.Symbol "(" loc1)) :: rest ->
            let
                expr =
                    Right (Arg [ expr_ ] { begin = loc1.begin, end = loc3.end })
            in
            { state | stack = expr :: rest } |> debugGreen "RED 3"

        -- RULE R4: R Expr fname args1, R A args2 -> Right Arg (args2 ++ arg1))
        (Right (Arg args2 loc2)) :: (Right (Expr fName args1 loc1)) :: rest ->
            let
                expr =
                    Right (Expr fName (args2 ++ args1) { begin = loc1.begin, end = loc2.end })
            in
            { state | stack = expr :: rest } |> debugGreen "RED 4"

        -- reduce  arg :: functionName :: rest to expr :: rest
        (Right (AST.Arg args loc2)) :: (Left (Token.FunctionName name loc1)) :: rest ->
            { state | stack = Right (AST.Expr name args { begin = loc1.begin, end = loc2.end }) :: rest } |> debugGreen "RED 5 (RULE B)"

        (Left (MarkedText "boldItalic" str loc)) :: [] ->
            reduceAux (Expr "boldItalic" [ AST.Text str loc ] loc) [] state |> debugGreen "RED 7"

        (Left (MarkedText "strong" str loc)) :: [] ->
            { state | committed = Expr "strong" [ AST.Text str loc ] loc :: state.committed, stack = [] } |> debugGreen "RED 8"

        (Left (MarkedText "italic" str loc)) :: [] ->
            { state | committed = Expr "italic" [ AST.Text str loc ] loc :: state.committed, stack = [] } |> debugGreen "RED 9"

        (Left (MarkedText "code" str loc)) :: [] ->
            { state | committed = AST.Verbatim "code" str loc :: state.committed, stack = [] } |> debugGreen "RED 10"

        (Left (MarkedText "math" str loc)) :: [] ->
            { state | committed = AST.Verbatim "math" str loc :: state.committed, stack = [] } |> debugGreen "RED 11"

        (Left (AnnotatedText "image" label value loc)) :: [] ->
            { state | committed = Expr "image" [ AST.Text value loc, AST.Text label loc ] loc :: state.committed, stack = [] } |> debugGreen "RED 12"

        (Left (AnnotatedText "link" label value loc)) :: [] ->
            { state | committed = Expr "link" [ AST.Text label loc, AST.Text value loc ] loc :: state.committed, stack = [] } |> debugGreen "RED 13"

        (Left (Special name argString loc)) :: [] ->
            { state | committed = Expr "special" [ AST.Text name loc, AST.Text argString loc ] loc :: state.committed, stack = [] } |> debugGreen "RED 14"

        _ ->
            state |> debugGreen "RED 15"


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux expr rest state =
    if rest == [] then
        { state | stack = [], committed = normalizeExpr expr :: state.committed } |> debugGreen "reduceAux, 1"

    else
        { state | stack = Right (normalizeExpr expr) :: rest } |> debugGreen "reduceAux, 2"


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

        (Right expr) :: [] ->
            Done { state | stack = [], committed = expr :: state.committed }

        _ ->
            let
                _ =
                    debugYellow "LAST EXIT, STACK" state.stack

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
