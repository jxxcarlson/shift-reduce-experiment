module LaTeX.Export.Markdown exposing (normalize)

import Block.Block exposing (Block(..), BlockStatus(..), ExprM(..))
import Markup.Meta


normalize : List Block -> List Block
normalize blocks =
    loop (init blocks) nextStep


type alias State =
    { input : List Block
    , output : List Block
    , stack : List Block
    , status : Status
    }


type Status
    = InsideList
    | OutsideList


init : List Block -> State
init blocks =
    { input = blocks
    , output = []
    , stack = []
    , status = OutsideList
    }


nextStep : State -> Step State (List Block)
nextStep state =
    case List.head state.input of
        Nothing ->
            Done state.output

        Just block ->
            Loop (nextState block state)


nextState : Block -> State -> State
nextState block state =
    case state.status of
        OutsideList ->
            case block of
                Block "item" _ _ ->
                    { state
                        | input = List.drop 1 state.input
                        , status = InsideList
                    }
                        |> Debug.log "XXX, OUTSIDE, FOUND ITEM"

                _ ->
                    { state
                        | input = List.drop 1 state.input
                        , output = block :: state.output
                    }
                        |> Debug.log "XXX, OUTSIDE, NO ITEM"

        InsideList ->
            case block of
                Block "item" [ Paragraph contents meta1 ] meta2 ->
                    let
                        newBlock =
                            Paragraph [ ExprM "item" contents Markup.Meta.dummy ] meta2
                    in
                    { state
                        | input = List.drop 1 state.input
                        , stack = newBlock :: state.stack
                    }
                        |> Debug.log "XXX, INSIDE, FOUND ITEM"

                _ ->
                    let
                        newBlock =
                            Block "itemize" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    { state
                        | input = List.drop 1 state.input
                        , stack = []
                        , status = OutsideList
                        , output = newBlock :: state.output
                    }
                        |> Debug.log "XXX, INSIDE, NO ITEM"


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
