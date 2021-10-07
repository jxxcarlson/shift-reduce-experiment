module Block.Library exposing (classify, finalize, processLine, recoverFromError)

import Block.L1.Line
import Block.Line exposing (LineData, LineType(..))
import Block.Markdown.Line
import Block.MiniLaTeX.Line
import Block.State exposing (State)
import Markup.Block exposing (SBlock(..))
import Markup.Debugger exposing (debug1, debug3)
import Markup.Tokenizer exposing (Lang(..))


finalize : State -> State
finalize state =
    { state | committed = reverseContents state.currentBlock :: state.committed |> List.reverse } |> debug3 "finalize"


recoverFromError : State -> State
recoverFromError state =
    { state | stack = [] } |> debug3 "recoverFromError "


{-|

    Function processLine determines the LineType of the given line
    using function classify.  After computing some auxilliary
    information, it passes the data to a dispatcher.  On the
    basis of the LineType, it then dispatches that data
    to a function defined in module Block.Handle. That function
    returns a new State value.

-}
processLine : Lang -> State -> State
processLine language state =
    case state.currentLineData.lineType of
        BeginBlock _ _ ->
            createBlock state

        BeginVerbatimBlock _ ->
            createBlock state

        EndBlock _ ->
            commitBlock state

        EndVerbatimBlock _ ->
            commitBlock state

        OrdinaryLine ->
            if state.previousLineData.lineType == BlankLine then
                createBlock state

            else
                case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        createBlock state

                    LT ->
                        commitBlock state

        VerbatimLine ->
            if state.previousLineData.lineType == VerbatimLine then
                addLineToCurrentBlock state

            else
                case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        addLineToCurrentBlock state

                    LT ->
                        commitBlock state

        BlankLine ->
            if state.previousLineData.lineType == BlankLine then
                state

            else
                case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        createBlock state

                    LT ->
                        commitBlock state

        Problem _ ->
            state


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


createBlockPhase1 : State -> State
createBlockPhase1 state =
    case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
        EQ ->
            commitBlock state

        GT ->
            shiftBlock state

        LT ->
            commitBlock state


createBlockPhase2 : State -> State
createBlockPhase2 state =
    (case state.currentLineData.lineType of
        OrdinaryLine ->
            { state
                | currentBlock =
                    SParagraph [ state.currentLineData.content ]
                        { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , blockCount = state.blockCount + 1
            }

        _ ->
            state
    )
        |> debug1 "createBlock "


commitBlock : State -> State
commitBlock state =
    if state.currentBlock == SSystem "initialBlock" then
        state

    else
        { state | committed = reverseContents state.currentBlock :: state.committed } |> debug1 "commitBlock"


shiftBlock : State -> State
shiftBlock state =
    { state | stack = state.currentBlock :: state.stack } |> debug1 "shiftBlock"


addLineToCurrentBlock : State -> State
addLineToCurrentBlock state =
    (case state.currentBlock of
        SParagraph lines meta ->
            { state | currentBlock = SParagraph (state.currentLineData.content :: lines) { meta | end = state.index } }

        _ ->
            state
    )
        |> debug1 "addLineToCurrentBlock"



-- HELPERS


classify : Lang -> Bool -> String -> LineData
classify language inVerbatimBlock str =
    let
        lineType =
            getLineTypeParser language

        leadingSpaces =
            Block.Line.countLeadingSpaces str

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str)

        lineType_ =
            if inVerbatimBlock && provisionalLineType == Block.Line.BlankLine then
                Block.Line.VerbatimLine

            else
                provisionalLineType
    in
    { indent = leadingSpaces, lineType = lineType_, content = str }


getLineTypeParser : Lang -> String -> Block.Line.LineType
getLineTypeParser language =
    case language of
        L1 ->
            Block.L1.Line.lineType

        Markdown ->
            Block.Markdown.Line.lineType

        MiniLaTeX ->
            Block.MiniLaTeX.Line.lineType


quantumOfIndentation =
    3


level : Int -> Int
level indentation =
    indentation // quantumOfIndentation


reverseContents : SBlock -> SBlock
reverseContents block =
    case block of
        SParagraph strings meta ->
            SParagraph (List.reverse strings) meta

        SVerbatimBlock name strings meta ->
            SVerbatimBlock name (List.reverse strings) meta

        SBlock name blocks meta ->
            SBlock name (List.map reverseContents blocks) meta

        SSystem s ->
            SSystem s
