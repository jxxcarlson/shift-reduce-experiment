module Block.Library exposing
    ( classify
    , finalize
    , processLine
    , recoverFromError
    , reduce
    , shiftCurrentBlock
    )

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools
import Block.Line exposing (BlockOption(..), LineData, LineType(..))
import Block.State exposing (Accumulator, State)
import Lang.Lang exposing (Lang(..))
import Lang.LineType.L1
import Lang.LineType.Markdown
import Lang.LineType.MiniLaTeX
import Markup.Debugger exposing (debug1, debug2, debug4)
import Markup.ParserTools
import Parser.Advanced
import Render.MathMacro


finalize : State -> State
finalize state =
    state |> identity |> finalizePhase2


finalizePhase2 : State -> State
finalizePhase2 state =
    case state.currentBlock of
        Nothing ->
            { state | committed = state.committed |> List.reverse } |> debug2 "finalize"

        Just block ->
            { state | committed = reverseContents block :: state.committed |> List.reverse } |> debug2 "finalize"


insertErrorMessage : State -> State
insertErrorMessage state =
    case state.errorMessage of
        Nothing ->
            state

        Just message ->
            { state
                | committed = SParagraph [ errorMessage state.lang message ] { status = BlockComplete, begin = 0, end = 0, id = "error", indent = 0 } :: state.committed
                , errorMessage = Nothing
            }


errorMessage : Lang -> { red : String, blue : String } -> String
errorMessage lang msg =
    case lang of
        L1 ->
            "[red " ++ msg.red ++ "]" ++ "[blue" ++ msg.blue ++ "]"

        Markdown ->
            "@red[" ++ msg.red ++ "] @blue[" ++ msg.blue ++ "]"

        MiniLaTeX ->
            "\\red{" ++ msg.red ++ "} \\skip{10} \\blue{" ++ msg.blue ++ "}"


recoverFromError : State -> State
recoverFromError state =
    { state | stack = [] } |> debug4 "recoverFromError "


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

        EndBlock name ->
            let
                currentlockName =
                    Maybe.andThen Block.BlockTools.sblockName state.currentBlock |> Maybe.withDefault "???"
            in
            if name == currentlockName then
                commitBlock { state | currentBlock = Maybe.map (Block.BlockTools.mapMeta (\meta -> { meta | status = BlockComplete })) state.currentBlock }

            else
                commitBlock
                    { state
                        | errorMessage =
                            Just { red = "Oops, the begin and end tags must match", blue = currentlockName ++ " ≠ " ++ name }
                    }

        EndVerbatimBlock name ->
            let
                currentlockName =
                    Maybe.andThen Block.BlockTools.sblockName state.currentBlock |> Maybe.withDefault "???"
            in
            if name == currentlockName then
                commitBlock { state | currentBlock = Maybe.map (Block.BlockTools.mapMeta (\meta -> { meta | status = BlockComplete })) state.currentBlock }

            else
                commitBlock
                    { state
                        | errorMessage =
                            Just { red = "Oops, the begin and end tags must match", blue = currentlockName ++ " ≠ " ++ name }
                    }

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
                        if state.verbatimBlockInitialIndent == state.previousLineData.indent then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> insertErrorMessage

                        else
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
                        if state.verbatimBlockInitialIndent == state.previousLineData.indent then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> insertErrorMessage

                        else
                            state |> commitBlock |> createBlock

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
                        case state.currentBlock of
                            Nothing ->
                                commitBlock state

                            Just block ->
                                let
                                    errorMessage_ =
                                        -- debug4 "BlankLine (LT)" (Just ("You need to terminate this block: begin{" ++ (Block.name block |> Maybe.withDefault "UNNAMED") ++ "}"))
                                        Just { red = "You need to terminate this block: ", blue = "\\texmacro{begin} \\texarg{" ++ (Block.BlockTools.sblockName block |> Maybe.withDefault "UNNAMED") ++ "}" }
                                in
                                commitBlock { state | errorMessage = errorMessage_ }

        Problem _ ->
            state


reduce : State -> State
reduce state =
    case state.stack of
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


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


createBlockPhase1 : State -> State
createBlockPhase1 state =
    case compare (level state.currentLineData.indent) (level state.previousLineData.indent) of
        LT ->
            case state.currentBlock of
                Nothing ->
                    commitBlock state

                Just _ ->
                    let
                        -- TODO: think about this
                        errorMessage_ =
                            debug4 "createBlockPhase1 (LT)" (Just { red = "You need to terminate this block (1)", blue = "??" })
                    in
                    commitBlock state

        EQ ->
            case state.currentBlock of
                Nothing ->
                    commitBlock state

                Just _ ->
                    let
                        -- TODO: think about this
                        errorMessage_ =
                            debug4 "createBlockPhase1 (EQ)" (Just { red = "You need to terminate this block (2)", blue = "??2" })
                    in
                    commitBlock state

        GT ->
            shiftCurrentBlock state


createBlockPhase2 : State -> State
createBlockPhase2 state =
    (case state.currentLineData.lineType of
        OrdinaryLine ->
            { state
                | currentBlock =
                    Just <|
                        SParagraph [ state.currentLineData.content ]
                            { begin = state.index, end = state.index, status = BlockComplete, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , blockCount = state.blockCount + 1
            }

        BeginBlock RejectFirstLine mark ->
            { state
                | currentBlock =
                    Just <|
                        SBlock mark
                            []
                            { begin = state.index, end = state.index, status = BlockIncomplete, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginBlock AcceptFirstLine _ ->
            { state
                | currentBlock =
                    Just <|
                        SBlock (nibble state.currentLineData.content |> transformHeading)
                            [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockComplete, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                            { begin = state.index, end = state.index, status = BlockIncomplete, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginBlock AcceptNibbledFirstLine kind ->
            { state
                | currentBlock =
                    Just <|
                        SBlock kind
                            [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockComplete, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                            { begin = state.index, end = state.index, status = BlockIncomplete, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginVerbatimBlock mark ->
            { state
                | currentBlock =
                    Just <|
                        SVerbatimBlock mark
                            []
                            { begin = state.index, end = state.index, status = BlockIncomplete, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , inVerbatimBlock = True
                , verbatimBlockInitialIndent = state.currentLineData.indent + quantumOfIndentation -- account for indentation of succeeding lines
                , blockCount = state.blockCount + 1
            }

        _ ->
            state
    )
        |> debug2 "createBlock "


commitBlock : State -> State
commitBlock state =
    state |> insertErrorMessage |> commitBlock_


commitBlock_ : State -> State
commitBlock_ state =
    case state.currentBlock of
        Nothing ->
            state

        Just block ->
            case List.head state.stack of
                Nothing ->
                    { state | committed = reverseContents block :: state.committed, currentBlock = Nothing, accumulator = updateAccumulator block state.accumulator } |> debug2 "commitBlock (1)"

                Just stackTop ->
                    case compare (levelOfBlock block) (levelOfBlock stackTop) of
                        GT ->
                            shiftBlock block state |> debug2 "commitBlock (2)"

                        EQ ->
                            { state | committed = block :: stackTop :: state.committed, stack = List.drop 1 state.stack, currentBlock = Nothing } |> debug1 "commitBlock (3)"

                        LT ->
                            { state | committed = block :: stackTop :: state.committed, stack = List.drop 1 state.stack, currentBlock = Nothing } |> debug1 "commitBlock (3)"


updateAccumulator : SBlock -> Accumulator -> Accumulator
updateAccumulator sblock1 accumulator =
    case sblock1 of
        SVerbatimBlock name contentList _ ->
            if name == "mathmacro" then
                { accumulator | macroDict = Render.MathMacro.makeMacroDict (String.join "\n" (List.map String.trimLeft contentList)) }

            else
                accumulator

        _ ->
            accumulator


shiftBlock : SBlock -> State -> State
shiftBlock block state =
    { state | stack = block :: state.stack, currentBlock = Nothing } |> debug2 "shiftBlock"


shiftCurrentBlock : State -> State
shiftCurrentBlock state =
    case state.currentBlock of
        Nothing ->
            state

        Just block ->
            shiftBlock block state |> debug2 "shiftCURRENTBlock"


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
        |> debug2 "addLineToCurrentBlock"



-- HELPERS


addLineToBlocks : Int -> LineData -> List SBlock -> List SBlock
addLineToBlocks index lineData blocks =
    case blocks of
        (SParagraph lines meta) :: rest ->
            SParagraph (lineData.content :: lines) { meta | end = index } :: rest

        rest ->
            -- TODO: the id field is questionable
            SParagraph [ lineData.content ] { status = BlockIncomplete, begin = index, end = index, id = String.fromInt index, indent = lineData.indent } :: rest


classify : Lang -> Bool -> Int -> String -> LineData
classify language inVerbatimBlock verbatimBlockInitialIndent str =
    let
        lineType =
            getLineTypeParser language

        leadingSpaces =
            Block.Line.countLeadingSpaces str

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str) |> debug2 "provisionalLineType"

        lineType_ =
            (-- if inVerbatimBlock && provisionalLineType == Block..BlankLine then
             if inVerbatimBlock && leadingSpaces >= (verbatimBlockInitialIndent |> debug2 "verbatimBlockInitialIndent") then
                Block.Line.VerbatimLine

             else
                provisionalLineType
            )
                |> debug2 "FINAL LINE TYPE"
    in
    { indent = leadingSpaces, lineType = lineType_, content = str }


getLineTypeParser : Lang -> String -> Block.Line.LineType
getLineTypeParser language =
    case language of
        L1 ->
            Lang.LineType.L1.lineType

        Markdown ->
            Lang.LineType.Markdown.lineType

        MiniLaTeX ->
            Lang.LineType.MiniLaTeX.lineType


quantumOfIndentation =
    3


levelOfBlock : SBlock -> Int
levelOfBlock block =
    case block of
        SParagraph _ meta ->
            level meta.indent

        SVerbatimBlock _ _ meta ->
            level meta.indent

        SBlock _ _ meta ->
            level meta.indent

        SError _ ->
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


transformHeading : String -> String
transformHeading str =
    case str of
        "#" ->
            "title"

        "##" ->
            "heading2"

        "###" ->
            "heading3"

        "####" ->
            "heading4"

        "#####" ->
            "heading5"

        _ ->
            str
