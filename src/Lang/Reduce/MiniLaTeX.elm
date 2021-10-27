module Lang.Reduce.MiniLaTeX exposing (recoverFromError, reduce, reduceFinal)

import Either exposing (Either(..))
import Expression.AST as AST exposing (Expr)
import Expression.ASTTools as ASTTools
import Expression.State exposing (State)
import Expression.Token as Token exposing (Token(..))
import Markup.Common exposing (Step(..))
import Markup.Debugger exposing (debugGreen, debugYellow)
import Expression.Stack as Stack exposing (Stack)
import List.Extra

fixedPoint : State -> State
fixedPoint state =
    let
        newState = reduce state
    in
    if state == newState then state |> debugGreen "EXIT FIXED POINT" else fixedPoint state |> debugGreen "FIXED POINT AGAIN"

reduceFinal : State -> State
reduceFinal state =
    state |> reduce |> reduceFinal_

reduceFinal_ : State -> State
reduceFinal_ state =
   ( let
        _ = debugYellow "reduceFinal_, IN" state
    in
    case state.stack of
        (Right (AST.Expr name args loc)) :: [] ->
            { state | committed = AST.Expr (transformMacroNames name) (List.reverse args) loc :: state.committed, stack = [] } |> debugGreen "FINAL RULE 1"

        (Left (FunctionName name loc)) :: rest ->
            -- { state | committed = AST.Expr (transformMacroNames name) [] loc :: state.committed, stack = [] } |> debug1 "FINAL RULE 2"
            let
                blue =
                    AST.Expr "blue" [ AST.Text ("\\" ++ String.trim name) loc ] loc

                red =
                    AST.Expr "errorHighlight" [ AST.Expr "red" [ AST.Text "{??}" loc ] loc ] loc
            in
            { state | committed = red :: blue :: state.committed, stack = rest }


        --(Left (Token.Text str loc)) :: rest ->
        --           {state | stack = rest, committed = (AST.Text str loc):: state.committed}  |> debugGreen "FINAL RULE 2"

        (Left (Token.Text str loc)) :: (Right expr):: [] ->
                   {state | stack = [], committed = (AST.Text str loc):: expr :: state.committed}  |> debugGreen "FINAL RULE 2"


        _ ->
            state  |> debugGreen "REDUCE FINAL, PASS"

  ) |> debugYellow "reduceFinal_, OUT"

{-|

    Using patterns of the form a :: b :: c ... :: [ ] instead of a :: b :: c ... :: rest makes
    the reduction process greedy.

-}
reduce : State -> State
reduce state =
    case state.stack of
        -- create a text expression from a text token, clearing the stack
        (Left (Token.Text str loc)) :: [] ->
            reduceAux (AST.Text str loc) [] state |> debugGreen "RULE 1"

        (Left (Token.Text str loc)) :: (Right expr):: [] ->
                   {state | stack = [], committed = (AST.Text str loc):: (AST.reverseContents expr) :: state.committed}  |> debugGreen "RULE X"

        -- Recognize an Expr
        (Left (Token.Symbol "}" loc4)) :: (Left (Token.Text arg loc3)) :: (Left (Token.Symbol "{" _)) :: (Left (Token.FunctionName name loc1)) :: rest ->
            { state | stack = Right (AST.Expr (transformMacroNames name) [ AST.Text arg loc3 ] { begin = loc1.begin, end = loc4.end }) :: rest } |> debugGreen "RULE 2"

        -- Merge a new Expr into an existing one
        (Left (Token.Symbol "}" loc4)) :: (Left (Token.Text arg loc3)) :: (Left (Token.Symbol "{" _)) :: (Right (AST.Expr name args loc1)) :: rest ->
            { state | stack = Right (AST.Expr (transformMacroNames name) (AST.Text arg loc3 :: args) { begin = loc1.begin, end = loc4.end }) :: rest } |> debugGreen "RULE 3"
        --
        ---- Merge new text into an existing Expr
        --(Left (Token.Text str loc2)) :: (Right (AST.Expr name args loc1)) :: rest ->
        --    { state | committed = AST.Text str loc2 :: AST.Expr (transformMacroNames name) (List.reverse args) loc1 :: state.committed, stack = rest } |> debugGreen "RULE 4"

        -- create a new expression from an existing one which occurs as a function argument
        (Left (Token.Symbol "}" loc4)) :: (Right (AST.Expr exprName args loc3)) :: (Left (Token.Symbol "{" _)) :: (Left (Token.FunctionName fName loc1)) :: rest ->
            { state | committed = AST.Expr fName [ AST.Expr (transformMacroNames exprName) args loc3 ] { begin = loc1.begin, end = loc4.end } :: state.committed, stack = rest } |> debugGreen "RULE 5"

         -- Transform "{" .... "}" to Right (Arg [....])
        (Left (Token.Symbol "}" _ ))::rest -> {state | stack = reduceArg state.stack } |> debugGreen "RULE A"


        (Right (AST.Arg args loc2)):: (Left (Token.FunctionName name loc1)):: rest ->
            {state | stack = (Right (AST.Expr name args {begin = loc1.begin, end = loc2.end})) :: rest}  |> debugGreen "RULE B"

        -- create a verbatim expression from a verbatim token, clearing the stack
        (Left (Token.Verbatim label content loc)) :: [] ->
            reduceAux (AST.Verbatim label content loc) [] state |> debugGreen "RULE 6"


        _ ->
            state






{-|

    If the stack is prefix::rest, where prefix = (L S "}", L a, L b, ...  , (L S "}"), convert it
    to newPrefix = Arg [R a, R b, ], and set the stack to newPrefix :: rest

-}
reduceArg : Stack -> Stack
reduceArg stack =
    let
        _ = debugYellow "reduce, IN" stack
    in
   ( case stack of
        (Left (Token.Symbol "}" loc2)) :: rest ->
            let
                _ = debugYellow "reduceArg" rest
                interior =
                    List.Extra.takeWhile (\item -> not (Stack.symbolToString item  == Just "{")) rest

                n =
                    List.length interior  |> debugYellow "n, interior length"

                found = List.Extra.getAt n rest |> debugYellow "found"
            in
            case ( List.Extra.getAt n rest, Stack.toExprList interior ) of
                ( Nothing, _ ) ->
                    stack

                ( _, Nothing ) ->
                    stack

                ( Just stackItem, Just exprList ) ->
                    case stackItem of
                        (Left (Token.Symbol "{" loc1)) ->
                            Right (AST.Arg ( exprList) {begin = loc1.begin, end = loc2.end}) :: List.drop (n + 1) rest

                        _ ->
                            stack

        _ ->
            stack) |> debugYellow "reduceArg (OUT)"

transformMacroNames : String -> String
transformMacroNames str =
    case str of
        "section" ->
            "heading1"

        "subsection" ->
            "heading2"

        "susubsection" ->
            "heading3"

        "subheading" ->
            "heading4"

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
        -- fix for macro with argument but no closing right brace
        (Left (Token.Text content loc3)) :: (Left (Symbol "{" loc2)) :: (Left (FunctionName name loc1)) :: rest ->
            let
                loc =
                    { begin = loc1.begin, end = loc3.end }

                blue =
                    AST.Expr "blue" [ AST.Text ("\\" ++ name ++ "{" ++ String.trim content) loc ] loc

                red =
                    AST.Expr "errorHighlight" [ AST.Expr "red" [ AST.Text "}" loc ] loc ] loc
            in
            Loop { state | committed = red :: blue :: state.committed, stack = rest }

        (Left (Token.Text content loc3)) :: (Left (Symbol "{" loc2)) :: (Right (AST.Expr name exprList loc1)) :: rest ->
            let
                loc =
                    { begin = loc1.begin, end = loc3.end }

                blue =
                    AST.Expr "blue" [ AST.Text ("\\" ++ name ++ AST.stringValueOfArgList (List.reverse exprList)) loc1 ] loc1

                red =
                    AST.Expr "errorHighlight" [ AST.Expr "red" [ AST.Text ("{" ++ String.trim content ++ "}") loc ] loc ] loc
            in
            Loop { state | committed = red :: blue :: state.committed, stack = rest }

        -- temporary fix for incomplete macro application
        (Left (Symbol "{" loc2)) :: (Left (FunctionName name loc1)) :: rest ->
            Loop { state | committed = AST.Expr "red" [ AST.Text ("\\" ++ name ++ "{??}") { begin = loc1.begin, end = loc2.end } ] { begin = loc1.begin, end = loc2.end } :: state.committed, stack = rest }

        -- commit text at the top of the stack
        --(Left (Token.Text content loc1)) :: rest ->
        --    let
        --        _ =
        --            debug4 "recover, RULE" 3
        --    in
        --    Loop { state | committed = AST.Text content loc1 :: state.committed, stack = rest }
        --(Left (Token.Text content loc1)) :: (Left (Symbol "{" _)) :: [] ->
        --    let
        --        _ =
        --            debug1 "RECOVER" 1
        --    in
        --    Loop
        --        { state
        --            | stack = Left (Symbol "}" loc1) :: state.stack
        --            , committed = AST.Expr "red" [ AST.Text content loc1 ] loc1 :: tstate.committed
        --        }
        (Left (Symbol "{" loc1)) :: (Left (Token.Text _ _)) :: (Left (Symbol "{" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "}" loc1) :: state.stack
                    , scanPointer = loc1.begin
                    , committed = AST.Text "I corrected an unmatched '{' in the following expression: " Token.dummyLoc :: state.committed
                }

        _ ->
            Done
                ({ state
                    | stack = Left (Symbol "}" { begin = state.scanPointer, end = state.scanPointer + 1 }) :: state.stack
                    , committed = AST.Expr "red" [ AST.Text (Stack.dump state.stack) Token.dummyLoc ] Token.dummyLoc :: state.committed
                 }
                    |> reduce
                    |> (\st -> { st | committed = List.reverse st.committed })
                )
