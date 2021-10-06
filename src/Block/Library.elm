module Block.Library exposing (finalize, processLine, recoverFromError, reduce)

import Block.Handle as Handle
import Block.L1.Line
import Block.Line exposing (BlockOption(..), LineData, LineType(..), countLeadingSpaces, emptyLineParser, ordinaryLineParser)
import Block.Markdown.Line
import Block.MiniLaTeX.Line
import Block.State exposing (State)
import Markup.Block exposing (Meta, SBlock(..))
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
    { state | stack = [] }


recoverFromError : State -> State
recoverFromError state =
    { state | stack = [] }


{-|

    Function processLine determines the LineType of the given line
    using function classify.  After computing some auxilliary
    information, it passes the data to a dispatcher.  On the
    basis of the LineType, it then dispatches that data
    to a function defined in module Block.Handle. That function
    returns a new State value.

-}
processLine : Lang -> String -> State -> State
processLine language line state =
    let
        lineData =
            classify language state.inVerbatimBlock line

        inVerbatimBlock =
            isInVerbatimBlock lineData state

        adjustedLineType =
            adjustLineType lineData inVerbatimBlock

        indent =
            lineData.indent
    in
    case adjustedLineType of
        BeginBlock AcceptFirstLine s ->
            Handle.beginBlock1 lineData indent state s

        BeginBlock RejectFirstLine s ->
            Handle.beginBlock2 indent state s

        BeginVerbatimBlock s ->
            Handle.beginVerbatimBlock indent state s

        OrdinaryLine ->
            state |> Handle.ordinaryLine indent line |> (\s -> { s | lineNumber = s.lineNumber + 1 })

        VerbatimLine ->
            state |> Handle.verbatimLine indent line

        BlankLine ->
            Handle.blankLine indent state

        EndBlock s ->
            Handle.endBlock indent state s

        EndVerbatimBlock s ->
            -- TODO: finish up
            reduce2 (EndVerbatimBlock s) state

        Problem s ->
            -- TODO: finish up
            state


shift : SBlock -> State -> State
shift block state =
    let
        newMeta =
            { begin = state.lineNumber
            , end = state.lineNumber
            , indent = state.indent
            , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
            }

        newBlock =
            replaceMeta newMeta block
    in
    { state
        | stack = newBlock :: state.stack
        , lineNumber = state.lineNumber + 1
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

        SError _ ->
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
    | E


typeOfBlock : SBlock -> SBlockType
typeOfBlock b =
    case b of
        SParagraph _ _ ->
            P

        SVerbatimBlock _ _ _ ->
            V

        SBlock _ _ _ ->
            B

        SError _ ->
            E


blockLabel : SBlock -> String
blockLabel block =
    case block of
        SParagraph _ _ ->
            "(no label)"

        SBlock s _ _ ->
            s

        SVerbatimBlock s _ _ ->
            s

        SError s ->
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

        SError s ->
            SError s


replaceMeta : Meta -> SBlock -> SBlock
replaceMeta meta block =
    case block of
        SParagraph strings _ ->
            SParagraph strings meta

        SVerbatimBlock name strings _ ->
            SVerbatimBlock name strings meta

        SBlock name blocks _ ->
            SBlock name blocks meta

        SError s ->
            SError s
