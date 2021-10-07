module Block.Library exposing
    ( classify
    , finalize
    , processLine
    , recoverFromError
    , reduce
    , shiftCurrentBlock
    )

import Block.L1.Line
import Block.Line exposing (BlockOption(..), LineData, LineType(..))
import Block.Markdown.Line
import Block.MiniLaTeX.Line
import Block.State exposing (State)
import Markup.Block exposing (SBlock(..))
import Markup.Debugger exposing (debug1, debug2, debug3)
import Markup.ParserTools
import Markup.Tokenizer exposing (Lang(..))
import Parser.Advanced


finalize : State -> State
finalize state =
    case state.currentBlock of
        Nothing ->
            { state | committed = state.committed |> List.reverse } |> debug3 "finalize"

        Just block ->
            { state | committed = reverseContents block :: state.committed |> List.reverse } |> debug3 "finalize"


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
                        createBlock state |> debug2 "CREATE BLOCK with ordinary line (GT)"

                    LT ->
                        state |> commitBlock |> createBlock

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


reduce : State -> State
reduce state =
    (case state.stack of
        block1 :: ((SBlock name blocks meta) as block2) :: rest ->
            if levelOfBlock block1 > levelOfBlock block2 then
                reduce { state | stack = SBlock name (block1 :: blocks) meta :: rest }

            else
                -- TODO: is this correct?
                reduce { state | committed = block1 :: block2 :: state.committed, stack = List.drop 2 state.stack }

        block :: [] ->
            { state | committed = reverseContents block :: state.committed, stack = [] }

        _ ->
            state
    )
        |> debug3 "REDUCE"


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


createBlockPhase1 : State -> State
createBlockPhase1 state =
    case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
        EQ ->
            commitBlock state

        GT ->
            shiftCurrentBlock state

        LT ->
            commitBlock state


createBlockPhase2 : State -> State
createBlockPhase2 state =
    (case state.currentLineData.lineType of
        OrdinaryLine ->
            { state
                | currentBlock =
                    Just <|
                        SParagraph [ state.currentLineData.content ]
                            { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , blockCount = state.blockCount + 1
            }

        BeginBlock RejectFirstLine mark ->
            { state
                | currentBlock =
                    Just <|
                        SBlock mark
                            []
                            { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginBlock AcceptFirstLine kind ->
            { state
                | currentBlock =
                    Just <|
                        SBlock kind
                            [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                            { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginVerbatimBlock mark ->
            { state
                | currentBlock =
                    Just <|
                        SVerbatimBlock mark
                            []
                            { begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , inVerbatimBlock = True
                , verbatimBlockInitialIndent = state.currentLineData.indent + quantumOfIndentation -- account for indentation of succeeding lines
                , blockCount = state.blockCount + 1
            }

        _ ->
            state
    )
        |> debug1 "createBlock "


commitBlock : State -> State
commitBlock state =
    case state.currentBlock of
        Nothing ->
            state

        Just block ->
            case List.head state.stack of
                Nothing ->
                    { state | committed = reverseContents block :: state.committed, currentBlock = Nothing } |> debug1 "commitBlock (1)"

                Just stackTop ->
                    case compare (levelOfBlock block) (levelOfBlock stackTop) of
                        GT ->
                            shiftBlock block state |> debug1 "commitBlock (2)"

                        EQ ->
                            { state | committed = block :: stackTop :: state.committed, stack = List.drop 1 state.stack, currentBlock = Nothing } |> debug1 "commitBlock (3)"

                        LT ->
                            { state | committed = block :: stackTop :: state.committed, stack = List.drop 1 state.stack, currentBlock = Nothing } |> debug1 "commitBlock (3)"


shiftBlock : SBlock -> State -> State
shiftBlock block state =
    { state | stack = block :: state.stack, currentBlock = Nothing } |> debug1 "shiftBlock"


shiftCurrentBlock : State -> State
shiftCurrentBlock state =
    case state.currentBlock of
        Nothing ->
            state

        Just block ->
            shiftBlock block state |> debug1 "shiftCURRENTBlock"


addLineToCurrentBlock : State -> State
addLineToCurrentBlock state =
    (case state.currentBlock of
        Nothing ->
            state

        Just (SParagraph lines meta) ->
            { state | currentBlock = Just <| SParagraph (state.currentLineData.content :: lines) { meta | end = state.index } }

        Just (SBlock mark blocks meta) ->
            { state | currentBlock = Just <| SBlock mark (addLineToBlocks state.index state.currentLineData blocks) { meta | end = state.index } }

        Just (SVerbatimBlock mark lines meta) ->
            { state | currentBlock = Just <| SVerbatimBlock mark (state.currentLineData.content :: lines) { meta | end = state.index } }

        _ ->
            state
    )
        |> debug1 "addLineToCurrentBlock"



-- HELPERS


addLineToBlocks : Int -> LineData -> List SBlock -> List SBlock
addLineToBlocks index lineData blocks =
    case blocks of
        (SParagraph lines meta) :: rest ->
            SParagraph (lineData.content :: lines) { meta | end = index } :: rest

        rest ->
            -- TODO: the id field is questionable
            SParagraph [ lineData.content ] { begin = index, end = index, id = String.fromInt index, indent = lineData.indent } :: rest


classify : Lang -> Bool -> Int -> String -> LineData
classify language inVerbatimBlock verbatimBlockInitialIndent str =
    let
        lineType =
            getLineTypeParser language

        leadingSpaces =
            Block.Line.countLeadingSpaces str |> debug3 "LEADING SPACES"

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str) |> debug1 "provisionalLineType"

        lineType_ =
            (-- if inVerbatimBlock && provisionalLineType == Block..BlankLine then
             if inVerbatimBlock && leadingSpaces >= (verbatimBlockInitialIndent |> debug2 "verbatimBlockInitialIndent") then
                Block.Line.VerbatimLine

             else
                provisionalLineType
            )
                |> debug1 "FINAL LINE TYPE"
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


levelOfBlock : SBlock -> Int
levelOfBlock block =
    case block of
        SParagraph strings meta ->
            level meta.indent

        SVerbatimBlock name strings meta ->
            level meta.indent

        SBlock name blocks meta ->
            level meta.indent

        SError str ->
            0


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
            SBlock name (List.reverse (List.map reverseContents blocks)) meta

        SError s ->
            SError s


incrementLevel : LineData -> LineData
incrementLevel lineData =
    { lineData | indent = lineData.indent + quantumOfIndentation }


setMetaEnd : Int -> Markup.Block.Meta -> Markup.Block.Meta
setMetaEnd k meta =
    { meta | end = k }


nibble : String -> String
nibble str =
    case Parser.Advanced.run (Markup.ParserTools.text (\c_ -> c_ /= ' ') (\c_ -> c_ /= ' ')) str of
        Ok stringData ->
            stringData.content

        Err _ ->
            ""


deleteSpaceDelimitedPrefix : String -> String
deleteSpaceDelimitedPrefix str =
    String.replace (nibble str ++ " ") "" str
