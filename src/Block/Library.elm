module Block.Library exposing (classify, finalize, processLine, recoverFromError, reduce)

import Block.L1.Line
import Block.Line exposing (BlockOption(..), LineData, LineType(..), countLeadingSpaces, emptyLineParser, ordinaryLineParser)
import Block.Markdown.Line
import Block.MiniLaTeX.Line
import Block.State exposing (State)
import Markup.Block exposing (Meta, SBlock(..))
import Markup.Debugger exposing (debug1, debug2, debug3)
import Markup.ParserTools as ParserTools
import Markup.Tokenizer exposing (Lang(..))


{-| Top-level functions:

        - reduce
        - processLine
        - finalize
        - recoverFromError

-}
reduce : State -> State
reduce state =
    { state | stack = [] }


reduce2 : LineType -> State -> State
reduce2 lineType state =
    { state | stack = [] }


finalize : State -> State
finalize state =
    { state | stack = [] } |> debug3 "finalize"


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
    let
        inVerbatimBlock =
            isInVerbatimBlock state.previousLineData state

        adjustedLineType =
            adjustLineType state.currentLineData inVerbatimBlock
    in
    case state.currentLineData.lineType of
        BeginBlock option str ->
            createBlock state

        BeginVerbatimBlock str ->
            createBlock state

        EndBlock str ->
            commitBlock state

        EndVerbatimBlock str ->
            commitBlock state

        OrdinaryLine ->
            if state.previousLineData.lineType == BlankLine then
                createBlock state

            else
                case compare (level state.currentLineData.indent) (level state.indent) of
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
                case compare (level state.currentLineData.indent) (level state.indent) of
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
                case compare (level state.currentLineData.indent) (level state.indent) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        createBlock state

                    LT ->
                        commitBlock state

        Problem str ->
            state


createBlock : State -> State
createBlock state =
    state |> debug1 "addNewBlock"


commitBlock : State -> State
commitBlock state =
    state |> debug1 "commitBlock"


addLineToCurrentBlock : State -> State
addLineToCurrentBlock state =
    state |> debug1 "addLineToCurrentBlock"


{-|

    Put new block on stack.

-}
shift : SBlock -> State -> State
shift block state =
    let
        newMeta =
            { begin = state.index
            , end = state.index
            , indent = state.indent
            , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
            }

        newBlock =
            replaceMeta newMeta block
    in
    { state
        | stack = newBlock :: state.stack
        , blockCount = state.blockCount + 1
    }



-- HELPERS


classify : Lang -> Bool -> String -> LineData
classify language inVerbatimBlock str =
    let
        lineType =
            getLineTypeParser language

        leadingSpaces =
            Block.Line.countLeadingSpaces str

        nibble str_ =
            String.dropLeft (String.length (ParserTools.nibble str_) + 1) str_

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str)

        lineType_ =
            if inVerbatimBlock && provisionalLineType == Block.Line.BlankLine then
                Block.Line.VerbatimLine

            else
                provisionalLineType
    in
    { indent = leadingSpaces, lineType = lineType_, content = nibble str }


getLineTypeParser : Lang -> String -> Block.Line.LineType
getLineTypeParser language =
    case language of
        L1 ->
            Block.L1.Line.lineType

        Markdown ->
            Block.Markdown.Line.lineType

        MiniLaTeX ->
            Block.MiniLaTeX.Line.lineType


adjustLineType lineType inVerbatimBlock =
    case lineType.lineType of
        BeginVerbatimBlock _ ->
            lineType.lineType

        BlankLine ->
            if inVerbatimBlock then
                BlankLine

            else
                BlankLine

        _ ->
            if inVerbatimBlock then
                VerbatimLine

            else
                lineType.lineType


isInVerbatimBlock lineType state =
    case lineType.lineType of
        BeginVerbatimBlock _ ->
            True

        _ ->
            -- TODO: check this out.  Is it OK?? Previously: < level state.indent
            if level lineType.indent < level state.verbatimBlockInitialIndent then
                False

            else
                state.inVerbatimBlock


quantumOfIndentation =
    3


level : Int -> Int
level indentation =
    indentation // quantumOfIndentation


blockLevel : SBlock -> Int
blockLevel block =
    case block of
        SParagraph _ meta ->
            level meta.indent

        SVerbatimBlock _ _ meta ->
            level meta.indent

        SBlock _ _ meta ->
            level meta.indent

        SSystem _ ->
            0


blockLevelOfStackTop : List SBlock -> Int
blockLevelOfStackTop stack =
    case List.head stack of
        Nothing ->
            0

        Just blockM ->
            blockLevel blockM


type SBlockType
    = P
    | V
    | B
    | S


typeOfBlock : SBlock -> SBlockType
typeOfBlock b =
    case b of
        SParagraph _ _ ->
            P

        SVerbatimBlock _ _ _ ->
            V

        SBlock _ _ _ ->
            B

        SSystem _ ->
            S


blockLabel : SBlock -> String
blockLabel block =
    case block of
        SParagraph _ _ ->
            "(no label)"

        SBlock s _ _ ->
            s

        SVerbatimBlock s _ _ ->
            s

        SSystem s ->
            s


blockLabelAtBottomOfStack : List SBlock -> String
blockLabelAtBottomOfStack stack =
    case List.head (List.reverse stack) of
        Nothing ->
            "(no label)"

        Just block ->
            blockLabel block


reverseStack : State -> State
reverseStack state =
    { state | stack = List.reverse state.stack }


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


replaceMeta : Meta -> SBlock -> SBlock
replaceMeta meta block =
    case block of
        SParagraph strings _ ->
            SParagraph strings meta

        SVerbatimBlock name strings _ ->
            SVerbatimBlock name strings meta

        SBlock name blocks _ ->
            SBlock name blocks meta

        SSystem s ->
            SSystem s
