module Block.Library exposing
    ( classify
    , processLine
    )

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools as BlockTools
import Block.Function as Function
import Block.Line exposing (BlockOption(..), LineData, LineType(..))
import Block.State exposing (Accumulator, State)
import LaTeX.MathMacro
import Lang.Lang exposing (Lang(..))
import Lang.LineType.L1
import Lang.LineType.Markdown
import Lang.LineType.MiniLaTeX
import Markup.Debugger exposing (debug3, debugBlue, debugCyan, debugMagenta, debugRed, debugYellow)
import Markup.ParserTools
import Parser.Advanced


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
            createBlock state |> debugOut "BeginBlock (OUT)"

        BeginVerbatimBlock mark ->
            if Just mark == Maybe.map getBlockName (Function.stackTop state) && (mark == "math" || mark == "code") then
                state |> endBlock mark

            else
                createBlock state |> debugOut "BeginVerbatimBlock (OUT)"

        EndBlock name ->
            endBlock name state

        EndVerbatimBlock name ->
            endBlock name state

        OrdinaryLine ->
            (if state.previousLineData.lineType == BlankLine then
                state |> Function.finalizeBlockStatusOfStackTop |> Function.simpleCommit |> createBlock

             else
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        case Function.stackTop state of
                            Nothing ->
                                state |> createBlock |> debugRed "TROUBLE HERE? (6)"

                            Just _ ->
                                Function.pushLineOntoStack state.index state.currentLineData.content state

                    GT ->
                        state |> addLineToStackTop |> debugRed "TROUBLE HERE? (2) — Add ordinary line to current block (GT)"

                    LT ->
                        if state.verbatimBlockInitialIndent == Function.levelOfCurrentBlock state then
                            addLineToStackTop { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> Function.insertErrorMessage

                        else
                            state |> commitBlock |> createBlock |> debugRed "TROUBLE HERE? (1)"
            )
                |> debugOut "OrdinaryLine (OUT)"

        VerbatimLine ->
            (if state.previousLineData.lineType == VerbatimLine then
                addLineToStackTop state

             else
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        addLineToStackTop state

                    GT ->
                        addLineToStackTop state

                    LT ->
                        -- TODO: is this OK?
                        if state.verbatimBlockInitialIndent == Function.levelOfCurrentBlock state then
                            addLineToStackTop { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> Function.insertErrorMessage

                        else
                            state |> commitBlock |> createBlock
            )
                |> debugOut "VerbatimLine (OUT)"

        BlankLine ->
            (if state.previousLineData.lineType == BlankLine then
                -- ignore the repeated blank line
                state |> debugYellow "BlankLine 0"

             else
                -- TODO.  Examine with care. I think this can all be reduced to index str state or commitBlock
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                    EQ ->
                        -- As long as the line is of level greater than or equal to
                        -- the level of the current verbatim block on top of the stack,
                        -- stuff those lines into the block
                        addLineToStackTop state |> debugYellow "BlankLine 1"

                    GT ->
                        -- as in the previous case
                        -- createBlock state |> debugYellow "BlankLine 2"
                        addLineToStackTop state |> debugYellow "BlankLine 1"

                    LT ->
                        -- TODO.   Can't this all be reduced to commitBlock?
                        case Function.stackTop state of
                            Nothing ->
                                commitBlock state |> debugYellow "BlankLine 3"

                            Just _ ->
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



-- CLASSIFY LINE


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



-- CREATE BLOCK


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


createBlockPhase1 : State -> State
createBlockPhase1 state =
    -- Determine whether we need to reduce the stack, pushing something onto committed
    case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
        LT ->
            case Function.stackTop state of
                Nothing ->
                    commitBlock state |> debugBlue "createBlockPhase1 (LT, NOTHING)" |> debugRed "TROUBLE HERE? (1)"

                Just _ ->
                    commitBlock state |> debugRed "TROUBLE HERE? (2)"

        EQ ->
            case Function.stackTop state of
                Nothing ->
                    commitBlock state

                Just _ ->
                    state |> Function.finalizeBlockStatusOfStackTop |> Function.simpleCommit |> debugRed "TROUBLE HERE? (3)"

        GT ->
            state


createBlockPhase2 : State -> State
createBlockPhase2 state =
    -- create a ne block
    (case state.currentLineData.lineType of
        OrdinaryLine ->
            let
                newBlock =
                    SParagraph [ state.currentLineData.content ] (newMeta state)
            in
            Function.pushBlock newBlock state

        BeginBlock RejectFirstLine mark ->
            let
                newBlock =
                    SBlock mark [] (newMeta state)
            in
            Function.pushBlock newBlock state

        BeginBlock AcceptFirstLine _ ->
            let
                newBlock =
                    SBlock (nibble state.currentLineData.content |> transformMarkdownHeading)
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] (newMeta state) ]
                        (newMeta state)
            in
            Function.pushBlock newBlock state

        BeginBlock AcceptNibbledFirstLine kind ->
            let
                newBlock =
                    SBlock kind
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] (newMeta state) ]
                        (newMeta state)
            in
            Function.pushBlock newBlock state

        BeginVerbatimBlock mark ->
            let
                newBlock =
                    SVerbatimBlock mark [] (newMeta state)
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


newMeta state =
    { begin = state.index
    , end = state.index
    , status = BlockStarted
    , id = String.fromInt state.blockCount
    , indent = state.currentLineData.indent
    }



-- ADD LINE TO BLOCK


addLineToStackTop : State -> State
addLineToStackTop state =
    (case Function.stackTop state of
        Nothing ->
            state

        Just (SParagraph _ _) ->
            Function.pushLineOntoStack state.index state.currentLineData.content state

        Just (SBlock mark blocks meta) ->
            let
                top =
                    SBlock mark (addLineToBlocks state.index state.currentLineData blocks) { meta | end = state.index }
            in
            { state | stack = top :: List.drop 1 state.stack }

        Just (SVerbatimBlock _ _ _) ->
            Function.pushLineOntoStack state.index state.currentLineData.content state

        _ ->
            state
    )
        |> debugSpare "addLineToCurrentBlock"


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



-- END BLOCK


endBlock name state =
    (case Function.nameOfStackTop state of
        Nothing ->
            -- the block is a paragraph, hence has no name
            state |> Function.changeStatusOfTopOfStack (MismatchedTags "anonymous" name) |> Function.simpleCommit

        Just stackTopName ->
            -- the begin and end tags match, so the block is complete; we commit it
            if name == stackTopName then
                state |> Function.changeStatusOfTopOfStack BlockComplete |> Function.simpleCommit

            else
                -- the tags don't match. We note that fact for the benefit of the renderer (or the error handler),
                -- and we commit the block
                state |> Function.changeStatusOfTopOfStack (MismatchedTags stackTopName name) |> Function.simpleCommit
    )
        |> debugOut "EndBlock (OUT)"



-- COMMIT


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

        top :: next :: _ ->
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



-- ACCUMULATOR


{-|

    This function post data to the accumulator field of State as the parser
    runs its loop.  That information is used downstream by the renderer.

-}
updateAccumulator : SBlock -> Accumulator -> Accumulator
updateAccumulator sblock1 accumulator =
    case sblock1 of
        SVerbatimBlock name contentList _ ->
            if name == "mathmacro" then
                { accumulator | macroDict = LaTeX.MathMacro.makeMacroDict (String.join "\n" (List.map String.trimLeft contentList)) } |> debug3 "Accumulator (1)"

            else
                accumulator |> debug3 "Accumlator (2)"

        _ ->
            accumulator |> debug3 "Accumulator (NADA)"



-- AST TRANSFORMATIONS


{-| transformHeading is used for Markdown so that we can have a single, simple AST
for all markup languages handled by the system -
-}
transformMarkdownHeading : String -> String
transformMarkdownHeading str =
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



-- ERROR HANDLING
-- HELPERS


getBlockName sblock =
    BlockTools.sblockName sblock |> Maybe.withDefault "UNNAMED"


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



-- DEBUG FUNCTIONS


debugSpare label state =
    state


debugOut label state =
    state
