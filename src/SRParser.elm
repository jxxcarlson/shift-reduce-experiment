module SRParser exposing (run)

import Either exposing (Either(..))
import Grammar exposing (GExpr(..))
import ParserTools
import Result exposing (Result)
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
    , stack : List (Either Token GExpr)
    , committed : List GExpr
    }


{-|

    > run "foo [i [j ABC]]"
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


nextState : State -> Step State State
nextState state =
    if state.scanPointer == state.end then
        Done (reduce state |> (\st -> { st | committed = List.reverse st.committed }))

    else
        nextState_ (reduce state)


nextState_ : State -> Step State State
nextState_ state =
    case Tokenizer.get (String.dropLeft state.scanPointer state.sourceText) of
        Err _ ->
            Done state

        Ok newToken ->
            Loop (shift newToken state)


shift : Token -> State -> State
shift token state =
    { state | scanPointer = state.scanPointer + Tokenizer.length token, stack = Either.Left token :: state.stack }


reduce : State -> State
reduce state =
    case state.stack of
        (Left (Text str)) :: [] ->
            { state | stack = [], committed = GText str :: state.committed }

        (Left (Symbol "]")) :: (Left (Text str)) :: (Left (Symbol "[")) :: rest ->
            reduceAux (makeGExpr str) rest state

        (Left (Symbol "]")) :: (Right gexpr) :: (Left (Text name)) :: (Left (Symbol "[")) :: rest ->
            reduceAux (makeGExpr2 name gexpr) rest state

        _ ->
            state


makeGExpr2 name gexpr =
    GExpr (String.trim name) [ gexpr ]


makeGExpr str =
    let
        words =
            String.words str

        prefix =
            List.head words |> Maybe.withDefault "empty"
    in
    GExpr prefix (List.map GText (List.drop 1 words))


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
