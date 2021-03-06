module Block.Parser exposing (run)

import Block.Block exposing (BlockStatus(..))
import Block.BlockTools as BlockTools
import Block.Function as Function
import Block.Library
import Block.State exposing (State)
import Lang.Lang exposing (Lang)
import List.Extra
import Markup.Debugger exposing (debugCyan, debugGreen, debugRed, debugYellow)
import Markup.Simplify as Simplify



-- TOP LEVEL FUNCTIONS


{-|

    This function runs the parser that maps the source text to a final State,
    one field of which is a List SBlock.

    So we get

        Source Text -> List SBlock

-}
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
        Loop ({ state | blockCount = state.blockCount + 1 } |> getLine lang |> Block.Library.processLine lang |> postProcess)


postProcess : State -> State
postProcess state =
    { state | previousLineData = state.currentLineData, index = state.index + 1 }


getLine : Lang -> State -> State
getLine language state =
    { state
        | currentLineData =
            Block.Library.classify language
                state.inVerbatimBlock
                state.initialBlockIndent
                (List.Extra.getAt state.index state.input
                    |> Maybe.withDefault "??"
                )
                |> debugCyan ("LINE DATA " ++ String.fromInt state.index)
    }


finalizeOrRecoverFromError : State -> Step State State
finalizeOrRecoverFromError state =
    state
        |> debug "finalizeOrRecoverFromError, reduce, IN"
        |> closeNonTerminatedBlocks
        |> Function.reduce
        |> debug "finalizeOrRecoverFromError, reduce, OUT"
        |> finalizeOrRecoverFromError_


{-| This function is needed for things like the quotation block
in Markdown which are "auto-termintated."
-}
closeNonTerminatedBlocks : State -> State
closeNonTerminatedBlocks state =
    case List.head state.stack of
        Nothing ->
            state

        Just top ->
            if List.member (BlockTools.sblockName top) [ Just "quotation" ] then
                Function.changeStatusOfTopOfStackRecursively BlockComplete state

            else
                state


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
