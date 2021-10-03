module SRParser exposing (run)

import Either exposing (Either(..))
import L1 exposing (Expr(..))
import Tokenizer exposing (Token(..))



{-
   https://discourse.elm-lang.org/t/parsers-with-error-recovery/6262/3
   https://www.cocolab.com/products/cocktail/doc.pdf/ell.pdf
   https://github.com/Janiczek/elm-grammar/tree/master/src
   https://guide.elm-lang.org/appendix/types_as_sets.html
   https://www.schoolofhaskell.com/user/bartosz/understanding-algebras
-}


type alias State =
    { sourceText : String
    , scanPointer : Int
    , end : Int
    , stack : List (Either Token Expr)
    , committed : List Expr
    }


{-|

    Run the parser on some input, returning a value of type state.
    The stack in the final state should be empty

    > SRParser.run "foo [i [j ABC]]"
    { committed = [GText ("foo "),GExpr "i" [GExpr "j" [GText "ABC"]]], end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }

-}
run : String -> State
run input =
    loop (init input) nextState


init : String -> State
init str =
    { sourceText = str
    , scanPointer = 0
    , end = String.length str
    , stack = []
    , committed = []
    }


{-|

    If scanPointer == end, you are done.
    Otherwise, get a new token from the source text, reduce the stack,
    and shift the new token onto the stack.

    NOTE: the

-}
nextState : State -> Step State State
nextState state_ =
    let
        state =
            reduce (state_ |> Debug.log "STATE")
    in
    if state.scanPointer >= state.end then
        -- Exit
        if state.stack == [] then
            Done (state |> (\st -> { st | committed = List.reverse st.committed }))

        else
            -- Or recover from error
            recoverFromError state

    else
        -- Grab a new token from the source text
        case Tokenizer.get state.scanPointer (String.dropLeft state.scanPointer state.sourceText) of
            Err _ ->
                -- Oops, exit
                Done state

            Ok newToken ->
                -- Process the token: reduce the stack, then shift the token onto it.
                Loop (shift newToken (reduce state))


recoverFromError : State -> Step State State
recoverFromError state =
    -- Use this when the loop is about to exit but the stack is non-empty.
    -- Look for error patterns on the top of the stack.
    -- If one is found, modify the stack and push an error message onto state.committed; then loop
    -- If no pattern is found, make a best effort: push (Left (Symbol "]")) onto the stack,
    -- push an error message onto state.committed, then exit as usual: apply function reduce
    -- to the state and reverse state.committed.
    case state.stack of
        (Left (Text _ loc1)) :: (Left (Symbol "[" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , committed = L1Text "I corrected an unmatched '[' in the following expression: " :: state.committed
                }

        (Left (Symbol "[" loc1)) :: (Left (Text _ _)) :: (Left (Symbol "[" _)) :: _ ->
            Loop
                { state
                    | stack = Left (Symbol "]" loc1) :: state.stack
                    , scanPointer = loc1.begin
                    , committed = L1Text "I corrected an unmatched '[' in the following expression: " :: state.committed
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
                    , committed = L1Text errorMessage :: state.committed
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
            Just (Tokenizer.startPositionOf token)

        Right _ ->
            Nothing


{-|

    Shift the new token onto the stack and advance the scan pointer

-}
shift : Token -> State -> State
shift token state =
    { state | scanPointer = state.scanPointer + Tokenizer.length token, stack = Either.Left token :: state.stack }


{-|

    Function reduce matches patterns at the top of the stack, then from the given instance
    of that pattern creates a GExpr.  Let the stack be (a::b::..::rest).  If rest
    is empty, push the new GExpr onto state.committed.  If not, push (Right GExpr)
    onto rest.  The stack now reads (Right GExpr)::rest.

    Note that the stack has type List (Either Token GExpr).

    NOTE: The pattern -> action clauses below invert productions in the grammar and so
    one should be able to deduce them mechanically from the grammar.

-}
reduce : State -> State
reduce state =
    case state.stack of
        (Left (Text str _)) :: [] ->
            reduceAux (L1Text str) [] state

        (Left (Math str _)) :: [] ->
            reduceAux (L1Math str) [] state

        (Left (Code str _)) :: [] ->
            reduceAux (L1Code str) [] state

        (Left (Symbol "]" _)) :: (Left (Text str _)) :: (Left (Symbol "[" _)) :: rest ->
            reduceAux (makeGExpr str) rest state

        (Left (Symbol "]" _)) :: (Right expr) :: (Left (Text name _)) :: (Left (Symbol "[" _)) :: rest ->
            reduceAux (makeGExpr2 name expr) rest state

        _ ->
            state


makeGExpr2 : String -> Expr -> Expr
makeGExpr2 name expr =
    Expr (String.trim name) [ expr ]


makeGExpr : String -> Expr
makeGExpr str =
    let
        words =
            String.words str

        prefix =
            List.head words |> Maybe.withDefault "empty"
    in
    Expr prefix (List.map L1Text (List.drop 1 words))


reduceAux : Expr -> List (Either Token Expr) -> State -> State
reduceAux newGExpr rest state =
    if rest == [] then
        { state | stack = [], committed = newGExpr :: state.committed }

    else
        { state | stack = Right newGExpr :: rest }


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b
