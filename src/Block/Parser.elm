module Block.Parser exposing (run)

import Block.Function as Function
import Block.Library
import Block.State exposing (State)
import Lang.Lang exposing (Lang)
import List.Extra
import Markup.Debugger exposing (debugBlue, debugCyan, debugGreen, debugMagenta, debugRed, debugYellow)
import Markup.Simplify as Simplify



-- TOP LEVEL FUNCTIONS


{-| -}
run : Lang -> Int -> List String -> State
run language generation input =
    loop (Block.State.init language generation input |> debugYellow "INITIAL STATE") (nextStep language)
        |> (\state -> { state | committed = List.reverse state.committed })


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
        finalizeOrRecoverFromError state |> debugRed "finalizeOrRecoverFromError"

    else
        Loop (state |> getLine lang |> Block.Library.processLine lang |> postProcess)


postProcess : State -> State
postProcess state =
    { state | previousLineData = state.currentLineData, index = state.index + 1 }



-- |> debug3 ("postProcess " ++ String.fromInt state.index)


getLine : Lang -> State -> State
getLine language state =
    { state
        | currentLineData =
            Block.Library.classify language
                state.inVerbatimBlock
                state.verbatimBlockInitialIndent
                (List.Extra.getAt state.index state.input
                    |> Maybe.withDefault "??"
                )
                |> debugCyan ("LINE DATA " ++ String.fromInt state.index)
    }


finalizeOrRecoverFromError : State -> Step State State
finalizeOrRecoverFromError state =
    state
        |> debug "finalizeOrRecoverFromError, reduce, IN"
        |> Function.reduce
        |> debug "finalizeOrRecoverFromError, reduce, OUT"
        |> finalizeOrRecoverFromError_


finalizeOrRecoverFromError_ : State -> Step State State
finalizeOrRecoverFromError_ state =
    if List.isEmpty state.stack then
        Done state
        --else if stackIsReducible state.stack then
        --    Loop (Block.Library.finalize state)

    else
        Loop (Function.recoverFromError state)



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



-- DEBUG


debugPrefix label state =
    let
        n =
            String.fromInt state.index ++ ". "
    in
    n ++ "(" ++ label ++ ") "


debug label state =
    let
        _ =
            debugGreen (debugPrefix label state ++ ", STACK") (state.stack |> List.map Simplify.sblock)

        _ =
            debugGreen (debugPrefix label state ++ ", COMM.") (state.committed |> List.map Simplify.sblock)
    in
    state
