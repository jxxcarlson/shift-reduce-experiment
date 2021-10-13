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
import Markup.Debugger exposing (debug1, debug2, debug3, debug4)
import Markup.ParserTools
import Markup.Simplify
import Parser.Advanced
import Render.MathMacro


finalize : State -> State
finalize state =
    state |> identity |> finalizePhase2 |> deBUG4 "finalize"


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
                | committed = SParagraph [ renderErrorMessage state.lang message ] { status = BlockComplete, begin = 0, end = 0, id = "error", indent = 0 } :: state.committed
                , errorMessage = Nothing
            }


renderErrorMessage : Lang -> { red : String, blue : String } -> String
renderErrorMessage lang msg =
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
            let
                _ =
                    deBUG4 "BeginBlock (IN)" state
            in
            createBlock state |> deBUG1 "BeginBlock (OUT)"

        BeginVerbatimBlock mark ->
            let
                _ =
                    deBUG4 "BeginVerbatimBlock (IN)" state
            in
            if Just mark == Maybe.map getBlockName state.currentBlock && (mark == "math" || mark == "code") then
                state |> endBlock mark

            else
                createBlock state |> deBUG4 "BeginVerbatimBlock (OUT)"

        EndBlock name ->
            endBlock name state

        --{ state
        --    --| errorMessage =
        --     --   Just { red = "Oops, the begin and end tags must match", blue = currentlockName ++ " ≠ " ++ name }
        --}
        EndVerbatimBlock name ->
            endBlock name state

        OrdinaryLine ->
            (let
                _ =
                    deBUG4 "OrdinaryLine (IN)" state
             in
             if state.previousLineData.lineType == BlankLine then
                createBlock state

             else
                case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
                    EQ ->
                        case state.currentBlock of
                            Nothing ->
                                state

                            Just currentBlock ->
                                case currentBlock of
                                    SParagraph lines meta ->
                                        state |> addLineToCurrentBlock

                                    SBlock _ _ _ ->
                                        state
                                            |> postErrorMessage "Error, current line has same level as the current block. I'll go ahead and add it to your block"
                                                "Hmmmm..."
                                            |> addLineToCurrentBlock

                                    SVerbatimBlock _ _ _ ->
                                        state
                                            |> postErrorMessage "Error, current line has same level as the current block. I'll go ahead and add it to your block"
                                                "Hmmmm......."
                                            |> addLineToCurrentBlock

                                    SError _ ->
                                        -- TODO: what should we do here?
                                        state

                    GT ->
                        state |> addLineToCurrentBlock |> debug2 "Add ordinary line to current block (GT)"

                    LT ->
                        if state.verbatimBlockInitialIndent == state.previousLineData.indent then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> insertErrorMessage

                        else
                            state |> commitBlock |> createBlock
            )
                |> deBUG1 "OrdinaryLine (OUT)"

        VerbatimLine ->
            (let
                _ =
                    deBUG4 "VerbatimLine (IN)" state
             in
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
            )
                |> deBUG4 "VerbatimLine (OUT)"

        BlankLine ->
            (let
                _ =
                    deBUG4 "BlankLine (IN)" state
             in
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
                                if state.lang == MiniLaTeX then
                                    state
                                        |> commitBlock

                                else
                                    state |> commitBlock
            )
                |> deBUG1 "BlankLine (OUT)"

        Problem _ ->
            let
                _ =
                    deBUG4 "Problem" state
            in
            state


endBlock name state =
    (let
        _ =
            debug3 "EndBlock, name" name

        _ =
            deBUG4 "EndBlock (IN)" state

        currentBlockName =
            Maybe.andThen Block.BlockTools.sblockName state.currentBlock |> Maybe.withDefault "???" |> debug3 "CURRENT BLOCK NAME"

        _ =
            debug3 "END TAG" name
     in
     if name == currentBlockName then
        commitBlock { state | currentBlock = Maybe.map (Block.BlockTools.mapMeta (\meta -> { meta | status = BlockComplete })) state.currentBlock }

     else
        { state | currentBlock = Maybe.map (Block.BlockTools.mapMeta (\meta -> { meta | status = MismatchedTags currentBlockName name })) state.currentBlock } |> commitBlock
    )
        |> deBUG4 "EndBlock (OUT)"


deBUG4 label state =
    let
        _ =
            debug4 (label ++ ": line") state.currentLineData

        _ =
            debug4 (label ++ ": block") state.currentBlock |> Maybe.map Markup.Simplify.sblock

        _ =
            debug4 (label ++ ": stack") state.stack

        _ =
            debug4 (label ++ ": committed") (state.committed |> List.map Markup.Simplify.sblock)
    in
    state


deBUG1 label state =
    let
        _ =
            debug1 (label ++ ": line") state.currentLineData

        _ =
            debug1 (label ++ ": block") state.currentBlock |> Maybe.map Markup.Simplify.sblock

        _ =
            debug1 (label ++ ": stack") state.stack

        _ =
            debug1 (label ++ ": committed") (state.committed |> List.map Markup.Simplify.sblock)
    in
    state


getBlockName sblock =
    Block.BlockTools.sblockName sblock |> Maybe.withDefault "UNNAMED"


postErrorMessage : String -> String -> State -> State
postErrorMessage red blue state =
    { state | errorMessage = Just { red = red, blue = blue } }


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
    case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
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
                            { begin = state.index, end = state.index, status = statusIncomplete state.lang "begin", id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginBlock AcceptFirstLine _ ->
            { state
                | currentBlock =
                    Just <|
                        SBlock (nibble state.currentLineData.content |> transformHeading)
                            [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockComplete, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                            { begin = state.index, end = state.index, status = statusIncomplete state.lang "begin", id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginBlock AcceptNibbledFirstLine kind ->
            { state
                | currentBlock =
                    Just <|
                        SBlock kind
                            [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockComplete, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                            { begin = state.index, end = state.index, status = statusIncomplete state.lang "begin", id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
                , currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , blockCount = state.blockCount + 1
            }

        BeginVerbatimBlock mark ->
            { state
                | currentBlock =
                    Just <|
                        SVerbatimBlock mark
                            []
                            { begin = state.index, end = state.index, status = statusIncomplete state.lang "begin", id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
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
    { state | stack = block :: state.stack, currentBlock = Nothing }


shiftCurrentBlock : State -> State
shiftCurrentBlock state =
    case state.currentBlock of
        Nothing ->
            state

        Just block ->
            shiftBlock block state |> deBUG4 "shiftCurrentBlock"


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
        |> deBUG1 "addLineToCurrentBlock"



-- HELPERS


statusIncomplete : Lang -> String -> BlockStatus
statusIncomplete lang message =
    case lang of
        L1 ->
            BlockComplete

        Markdown ->
            BlockComplete

        MiniLaTeX ->
            BlockIncomplete message


addLineToBlocks : Int -> LineData -> List SBlock -> List SBlock
addLineToBlocks index lineData blocks =
    case blocks of
        (SParagraph lines meta) :: rest ->
            -- there was a leading paragraph, so we can prepend the line lineData.content
            SParagraph (lineData.content :: lines) { meta | end = index } :: rest

        rest ->
            -- TODO: the id field is questionable
            -- otherwise we prepend a paragraph with the given line
            SParagraph [ lineData.content ] { status = BlockComplete, begin = index, end = index, id = String.fromInt index, indent = lineData.indent } :: rest


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


levelOfCurrentBlock : State -> Int
levelOfCurrentBlock state =
    case state.currentBlock of
        Nothing ->
            -1

        Just block ->
            levelOfBlock block


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
