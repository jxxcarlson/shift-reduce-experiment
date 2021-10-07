module Block.Parser exposing (runParser)

import Block.Library
import Block.Line
import Block.State exposing (State)
import Dict exposing (Dict)
import List.Extra
import Markup.Block
import Markup.Debugger exposing (debug1, debug2, debug3)
import Markup.Tokenizer exposing (Lang)



-- TOP LEVEL FUNCTIONS


{-| -}
runParser : Lang -> Int -> List String -> State
runParser language generation input =
    loop (Block.State.init generation input |> debug3 "INITIAL STATE") (nextStep language)


{-|

    nextStep depends on four top-level functions in Block.Library:

        - reduce
        - processLine
        - finalize
        - recoverFromError

-}
nextStep : Lang -> State -> Step State State
nextStep lang state =
    if state.index >= state.lastIndex then
        finalizeOrRecoverFromError state

    else
        Loop (state |> getLine lang |> Block.Library.processLine lang |> postProcess)


postProcess : State -> State
postProcess state =
    { state | previousLineData = state.currentLineData }


getLine : Lang -> State -> State
getLine language state =
    let
        line =
            List.Extra.getAt state.index state.input |> Maybe.withDefault ""
    in
    { state
        | currentLineData =
            Block.Library.classify language
                state.inVerbatimBlock
                (List.Extra.getAt state.index state.input
                    |> Maybe.withDefault "??"
                )
                |> debug2 ("LINE DATA " ++ String.fromInt state.index)
        , index = state.index + 1
    }


finalizeOrRecoverFromError : State -> Step State State
finalizeOrRecoverFromError state =
    if List.isEmpty state.stack then
        Done (Block.Library.finalize state)

    else
        Loop (Block.Library.recoverFromError state)



-- HELPERS


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
