module LaTeX.Export.Markdown exposing (normalize)

import Block.Block exposing (Block(..), BlockStatus(..), ExprM(..))
import Markup.Meta


normalize : List Block -> List Block
normalize blocks =
    loop (init blocks) nextStep |> List.reverse


type alias State =
    { input : List Block
    , output : List Block
    , stack : List Block
    , status : Status
    }


type Status
    = InsideList
    | InsideNumberedList
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
            case state.status of
                InsideList ->
                    let
                        newBlock =
                            Block "itemize" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    Done (newBlock :: state.output)

                InsideNumberedList ->
                    let
                        newBlock =
                            Block "enumerate" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    Done (newBlock :: state.output)

                OutsideList ->
                    Done state.output

        Just block ->
            Loop (nextState block state)


nextState : Block -> State -> State
nextState block state =
    case state.status of
        OutsideList ->
            case block of
                Paragraph [ ExprM "item" _ _ ] _ ->
                    { state
                        | input = List.drop 1 state.input
                        , status = InsideList
                        , stack = block :: []
                    }

                Paragraph [ ExprM "numberedItem" _ _ ] _ ->
                    { state
                        | input = List.drop 1 state.input
                        , status = InsideNumberedList
                        , stack = block :: []
                    }

                _ ->
                    { state
                        | input = List.drop 1 state.input
                        , output = block :: state.output
                    }

        InsideList ->
            case block of
                Paragraph [ ExprM "item" _ _ ] _ ->
                    { state
                        | input = List.drop 1 state.input
                        , stack = block :: state.stack
                    }

                Paragraph [ ExprM "numberedItem" _ _ ] _ ->
                    let
                        newBlock =
                            Block "itemize" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    { state
                        | input = List.drop 1 state.input
                        , status = InsideNumberedList
                        , output = newBlock :: state.output
                        , stack = block :: []
                    }

                _ ->
                    let
                        newBlock =
                            Block "itemize" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    { state
                        | input = List.drop 1 state.input
                        , stack = []
                        , status = OutsideList
                        , output = block :: newBlock :: state.output
                    }

        InsideNumberedList ->
            case block of
                Paragraph [ ExprM "numberedItem" _ _ ] _ ->
                    { state
                        | input = List.drop 1 state.input
                        , stack = block :: state.stack
                    }

                Paragraph [ ExprM "item" _ _ ] _ ->
                    let
                        newBlock =
                            Block "enumerate" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    { state
                        | input = List.drop 1 state.input
                        , status = InsideList
                        , output = newBlock :: state.output
                        , stack = block :: []
                    }

                _ ->
                    let
                        newBlock =
                            Block "enumerate" (List.reverse state.stack) Block.Block.dummyMeta
                    in
                    { state
                        | input = List.drop 1 state.input
                        , stack = []
                        , status = OutsideList
                        , output = block :: newBlock :: state.output
                    }


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
