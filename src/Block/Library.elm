module Block.Library exposing
    ( classify
    , processLine
    )

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools as BlockTools
import Block.Function as Function
import Block.Line exposing (BlockOption(..), LineData, LineType(..))
import Block.State exposing (Accumulator, State)
import Lang.Lang exposing (Lang(..))
import Lang.LineType.L1
import Lang.LineType.Markdown
import Lang.LineType.MiniLaTeX
import Markup.Debugger exposing (debugBlue, debugCyan, debugMagenta, debugRed, debugYellow)
import Markup.ParserTools
import Markup.Simplify
import Parser.Advanced
import Render.MathMacro


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
                    debugIn "BeginBlock (IN)" state
            in
            createBlock state |> debugOut "BeginBlock (OUT)"

        BeginVerbatimBlock mark ->
            let
                _ =
                    debugIn "BeginVerbatimBlock (IN)" state
            in
            if Just mark == Maybe.map getBlockName (Function.stackTop state) && (mark == "math" || mark == "code") then
                state |> endBlock mark

            else
                createBlock state |> debugOut "BeginVerbatimBlock (OUT)"

        EndBlock name ->
            endBlock name state

        EndVerbatimBlock name ->
            endBlock name state

        OrdinaryLine ->
            (let
                _ =
                    debugIn "OrdinaryLine (IN)" state
             in
             if state.previousLineData.lineType == BlankLine then
                state |> Function.finalizeBlockStatusOfStackTop |> Function.simpleCommit |> createBlock

             else
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        case Function.stackTop state of
                            Nothing ->
                                state |> createBlock |> debugRed "TROUBLE HERE? (6)"

                            Just topBlock ->
                                case topBlock of
                                    SParagraph lines meta ->
                                        state |> addLineToCurrentBlock |> debugRed "TROUBLE HERE? (5)"

                                    SBlock _ _ _ ->
                                        state
                                            |> postErrorMessage "" "Indent lines of following block"
                                            |> addLineToCurrentBlock
                                            |> debugRed "TROUBLE HERE? (4)"

                                    SVerbatimBlock _ _ _ ->
                                        state
                                            |> postErrorMessage "" "Indent lines of following block"
                                            |> addLineToCurrentBlock
                                            |> debugRed "TROUBLE HERE? (3) !!!!"

                                    SError _ ->
                                        -- TODO: what should we do here?
                                        state

                    GT ->
                        state |> addLineToCurrentBlock |> debugRed "TROUBLE HERE? (2) — Add ordinary line to current block (GT)"

                    LT ->
                        if state.verbatimBlockInitialIndent == Function.levelOfCurrentBlock state then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> Function.insertErrorMessage

                        else
                            state |> commitBlock |> createBlock |> debugRed "TROUBLE HERE? (1)"
            )
                |> debugOut "OrdinaryLine (OUT)"

        VerbatimLine ->
            (let
                _ =
                    debugIn "VerbatimLine (IN)" state
             in
             if state.previousLineData.lineType == VerbatimLine then
                addLineToCurrentBlock state

             else
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        addLineToCurrentBlock state

                    LT ->
                        if state.verbatimBlockInitialIndent == Function.levelOfCurrentBlock state then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> Function.insertErrorMessage

                        else
                            state |> commitBlock |> createBlock
            )
                |> debugOut "VerbatimLine (OUT)"

        BlankLine ->
            (let
                _ =
                    debugIn "BlankLine (IN)" state
             in
             if state.previousLineData.lineType == BlankLine then
                state |> debugYellow "BlankLine 0"

             else
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        addLineToCurrentBlock state |> debugYellow "BlankLine 1"

                    GT ->
                        createBlock state |> debugYellow "BlankLine 2"

                    LT ->
                        case Function.stackTop state of
                            Nothing ->
                                commitBlock state |> debugYellow "BlankLine 3"

                            Just block ->
                                if state.lang == MiniLaTeX then
                                    state
                                        |> commitBlock
                                        |> debugYellow "BlankLine 4"

                                else
                                    state |> commitBlock |> debugYellow "BlankLine 5"
            )
                |> debugOut "BlankLine (OUT)"

        Problem _ ->
            state


endBlock name state =
    (let
        _ =
            debugYellow "EndBlock, name" name

        _ =
            debugIn "EndBlock (IN)" state
     in
     case Function.nameOfStackTop state of
        Nothing ->
            state |> Function.changeStatusOfTopOfStack (MismatchedTags "anonymous" name) |> Function.simpleCommit

        Just stackTopName ->
            if name == stackTopName then
                state |> Function.changeStatusOfTopOfStack BlockComplete |> Function.simpleCommit

            else
                state |> Function.changeStatusOfTopOfStack (MismatchedTags stackTopName name) |> Function.simpleCommit
    )
        |> debugOut "EndBlock (OUT)"


debugSpare label state =
    let
        n =
            String.fromInt state.index ++ ". "

        _ =
            debugBlue (n ++ label ++ ": line") state.currentLineData

        _ =
            debugYellow (n ++ label ++ ": stack") (state.stack |> List.map Markup.Simplify.sblock)

        _ =
            debugRed (n ++ label ++ ": committed") (state.committed |> List.map Markup.Simplify.sblock)
    in
    state


debugIn label state =
    let
        n =
            String.fromInt state.index ++ ". "

        _ =
            debugBlue (n ++ label ++ ": line") state.currentLineData

        _ =
            debugYellow (n ++ label ++ ": stack") (state.stack |> List.map Markup.Simplify.sblock)

        _ =
            debugRed (n ++ label ++ ": committed") (state.committed |> List.map Markup.Simplify.sblock)
    in
    state


debugOut label state =
    let
        n =
            String.fromInt state.index ++ ". "

        _ =
            debugYellow (n ++ label ++ ": stack") (state.stack |> List.map Markup.Simplify.sblock)

        _ =
            debugRed (n ++ label ++ ": committed") (state.committed |> List.map Markup.Simplify.sblock)
    in
    state


getBlockName sblock =
    BlockTools.sblockName sblock |> Maybe.withDefault "UNNAMED"


postErrorMessage : String -> String -> State -> State
postErrorMessage red blue state =
    { state | errorMessage = Just { red = red, blue = blue } }


createBlockPhase1 : State -> State
createBlockPhase1 state =
    -- Determine whether we need to reduce the stack, pushing something onto committed
    case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
        LT ->
            case Function.stackTop state of
                Nothing ->
                    commitBlock state |> debugBlue "createBlockPhase1 (LT, NOTHING)" |> debugRed "TROUBLE HERE? (1)"

                Just _ ->
                    let
                        -- TODO: think about this
                        errorMessage_ =
                            debugBlue "createBlockPhase1 (LT)" (Just { red = "You need to terminate this block (1)", blue = "??" })
                    in
                    commitBlock state |> debugRed "TROUBLE HERE? (2)"

        EQ ->
            case Function.stackTop state of
                Nothing ->
                    commitBlock state

                Just _ ->
                    state |> Function.finalizeBlockStatusOfStackTop |> Function.simpleCommit |> debugRed "TROUBLE HERE? (3)"

        GT ->
            state |> debugRed "TROUBLE HERE? (4)"


createBlockPhase2 : State -> State
createBlockPhase2 state =
    -- create a ne block
    (case state.currentLineData.lineType of
        OrdinaryLine ->
            let
                newBlock =
                    SParagraph [ state.currentLineData.content ] { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            Function.pushBlock newBlock state

        BeginBlock RejectFirstLine mark ->
            let
                newBlock =
                    SBlock mark [] { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            Function.pushBlock newBlock state

        BeginBlock AcceptFirstLine _ ->
            let
                newBlock =
                    SBlock (nibble state.currentLineData.content |> transformHeading)
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockStarted, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            Function.pushBlock newBlock state

        BeginBlock AcceptNibbledFirstLine kind ->
            let
                newBlock =
                    SBlock kind
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockStarted, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            Function.pushBlock newBlock state

        BeginVerbatimBlock mark ->
            let
                newBlock =
                    SVerbatimBlock mark
                        []
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            { state
                | currentLineData = Function.incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , inVerbatimBlock = True
                , verbatimBlockInitialIndent = state.currentLineData.indent + Function.quantumOfIndentation -- account for indentation of succeeding lines
                , blockCount = state.blockCount + 1
            }
                |> Function.pushBlock newBlock

        _ ->
            state
    )
        |> debugCyan "createBlock "


commitBlock : State -> State
commitBlock state =
    state |> Function.insertErrorMessage |> commitBlock_


commitBlock_ : State -> State
commitBlock_ state =
    let
        finalize_ =
            Function.finalizeBlockStatus >> Function.reverseContents
    in
    case state.stack of
        [] ->
            state

        top :: [] ->
            let
                top_ =
                    finalize_ top
            in
            { state
                | committed = top_ :: state.committed
                , accumulator = updateAccumulator top_ state.accumulator
            }
                |> debugCyan "commitBlock (1)"

        top :: next :: rest ->
            let
                top_ =
                    finalize_ top

                next_ =
                    finalize_ next
            in
            case compare (Function.levelOfBlock top) (Function.levelOfBlock next) of
                GT ->
                    Function.shiftBlock top_ state |> debugCyan "commitBlock (2)"

                EQ ->
                    { state | committed = top_ :: next_ :: state.committed, stack = List.drop 1 state.stack } |> debugMagenta "commitBlock (3)"

                LT ->
                    { state | committed = top_ :: next_ :: state.committed, stack = List.drop 1 state.stack } |> debugMagenta "commitBlock (4)"


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


addLineToCurrentBlock : State -> State
addLineToCurrentBlock state =
    (case Function.stackTop state of
        Nothing ->
            state

        Just (SParagraph lines meta) ->
            Function.pushLineOntoStack state.index state.currentLineData.content state

        Just (SBlock mark blocks meta) ->
            let
                top =
                    SBlock mark (addLineToBlocks state.index state.currentLineData blocks) { meta | end = state.index }
            in
            { state | stack = top :: List.drop 1 state.stack }

        Just (SVerbatimBlock mark lines meta) ->
            Function.pushLineOntoStack state.index state.currentLineData.content state

        _ ->
            state
    )
        |> debugSpare "addLineToCurrentBlock"



-- HELPERS


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
            lineType (String.dropLeft leadingSpaces str) |> debugCyan "provisionalLineType"

        lineType_ =
            (if inVerbatimBlock && leadingSpaces >= (verbatimBlockInitialIndent |> debugCyan "verbatimBlockInitialIndent") then
                Block.Line.VerbatimLine

             else
                provisionalLineType
            )
                |> debugCyan "FINAL LINE TYPE"
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


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


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
