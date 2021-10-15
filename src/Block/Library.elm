module Block.Library exposing
    ( classify
    , processLine
    )

import Block.Block as Block exposing (BlockStatus(..), SBlock(..))
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
import Markup.Simplify as Simplify
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
            state
                |> debugIn "BeginBlock"
                |> Function.pushBlockOnState
                |> debugOut "BeginBlock (OUT)"

        BeginVerbatimBlock mark ->
            let
                _ =
                    debugIn "BeginVerbatimBlock" state
            in
            (if Just mark == Maybe.map getBlockName (Function.stackTop state) && (mark == "math" || mark == "code") then
                state |> endBlock mark

             else
                Function.pushBlockOnState state
            )
                |> debugOut "BeginVerbatimBlock (OUT)"

        EndBlock name ->
            let
                _ =
                    debugIn "EndBlock" state
            in
            endBlock name state

        EndVerbatimBlock name ->
            let
                _ =
                    debugIn "EndVerbatimBlock" state
            in
            endBlock name state

        OrdinaryLine ->
            let
                _ =
                    debugIn "OrdinaryLine" state
            in
            case Function.stackTop state of
                Nothing ->
                    let
                        _ =
                            debugRed "Nothing branch of OrdinaryLine" state
                    in
                    state
                        |> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta state))
                        |> debugOut "End of Nothing Branch, OrdinaryLine"

                Just top ->
                    (if state.previousLineData.lineType == BlankLine then
                        -- A non-blank line followed a blank one, so create a new paragraph
                        state
                            |> Function.finalizeBlockStatusOfStackTop
                            |> Function.simpleCommit
                            |> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta state))

                     else
                        -- Handle the case of a non-blank line following a non-blank line.
                        -- The action depends on the indentation of the current line as compared to
                        -- the level of the current block (top of the stack)
                        let
                            _ =
                                debugRed "(i, j)" ( Function.level state.currentLineData.indent, Function.levelOfCurrentBlock state )
                        in
                        case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state) of
                            EQ ->
                                -- If the block on top of the stack is a paragraph, add the
                                -- current line to it.
                                if Block.typeOfSBlock top == Block.P then
                                    Function.pushLineOntoStack state.index state.currentLineData.content state

                                else
                                    -- Otherwise, commit the top block and create
                                    -- a new paragraph with the current line
                                    state
                                        |> Function.liftBlockFunctiontoStateFunction Function.finalizeBlockStatus
                                        |> Function.simpleCommit
                                        |> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta state))

                            GT ->
                                -- The line has greater level than the block on top of the stack, so add it to the block
                                -- TODO. Or should we create a new block?
                                state |> addLineToStackTop |> debugRed "TROUBLE HERE? (2) — Add ordinary line to current block (GT)"

                            LT ->
                                -- If the block on top of the stack is a verbatim block and the indentation
                                -- of the current line is less than the indentation level of the block,
                                -- then signal an error but add it to the block anyway.  Otherwise, commit
                                -- the current block and create a new one.
                                -- TODO. In fact, in the else clause, we should reduce the stack, then create the block.
                                if state.verbatimBlockInitialIndent == Function.levelOfCurrentBlock state then
                                    addLineToStackTop
                                        { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                        |> Function.insertErrorMessage

                                else
                                    state
                                        |> debugOut "OrdinaryLine, ELSE clause (1)"
                                        |> Function.liftBlockFunctiontoStateFunction (Function.finalizeBlockStatus >> Function.reverseContents)
                                        |> debugOut "OrdinaryLine, ELSE clause (2)"
                                        |> Function.simpleCommit
                                        |> debugOut "OrdinaryLine, ELSE clause (3)"
                                        |> Function.pushLineOntoStack state.index state.currentLineData.content
                                        |> debugOut "OrdinaryLine, ELSE clause (4)"
                    )
                        |> debugOut "OrdinaryLine (OUT)"

        VerbatimLine ->
            let
                _ =
                    debugIn "VerbatimLine" state
            in
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
                            state
                                |> addLineToStackTop
                                |> Function.postErrorMessage
                                    "Below: you forgot to indent the block text."
                                    ""
                                |> Function.insertErrorMessage

                        else
                            -- The indentation level is too small.  Commit the block on
                            -- top of the stack and create a new block.
                            state
                                |> commitBlock
                                |> Function.pushBlockOnState
            )
                |> debugOut "VerbatimLine (OUT)"

        BlankLine ->
            let
                _ =
                    debugIn "BlankLine" state
            in
            (if state.previousLineData.lineType == BlankLine then
                -- ignore the repeated blank line
                state |> debugYellow "BlankLine 0"

             else
                -- TODO.  Examine with care. I think this can all be reduced to index str state or commitBlock
                let
                    _ =
                        debugRed "(i, j)" ( Function.level state.currentLineData.indent, Function.levelOfCurrentBlock state + 1 )

                    _ =
                        debugRed "STACK TOP" (List.head state.stack)
                in
                case compare (Function.level state.currentLineData.indent) (Function.levelOfCurrentBlock state + 1) of
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
                                        |> Function.finalizeBlockStatusOfStackTop
                                        |> Function.simpleCommit
                                        |> debugYellow "BlankLine 4"

                                else
                                    state
                                        |> Function.finalizeBlockStatusOfStackTop
                                        |> Function.simpleCommit
                                        |> debugYellow "BlankLine 5"
            )
                |> debugOut "BlankLine (OUT)"

        Problem _ ->
            let
                _ =
                    debugIn "Problem" state
            in
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


endBlock : String -> State -> State
endBlock name state =
    -- TODO: This function will have to be revisited: the block ending may arrive with the matching
    -- block deep in the stack.
    -- updateAccummulatorInStateWithBlock
    case List.head state.stack of
        Nothing ->
            state

        -- This is an error, to (TODO) we need to figure out what to do.
        Just top ->
            (case Function.nameOfStackTop state of
                Nothing ->
                    -- the block is a paragraph, hence has no name
                    state |> Function.changeStatusOfTopOfStack (MismatchedTags "anonymous" name) |> Function.simpleCommit

                Just stackTopName ->
                    -- the begin and end tags match, so the block is complete; we commit it
                    if name == stackTopName then
                        state
                            |> updateAccummulatorInStateWithBlock top
                            |> Function.changeStatusOfTopOfStack BlockComplete
                            |> Function.simpleCommit

                    else
                        -- the tags don't match. We note that fact for the benefit of the renderer (or the error handler),
                        -- and we commit the block
                        state
                            |> updateAccummulatorInStateWithBlock top
                            |> Function.changeStatusOfTopOfStack (MismatchedTags stackTopName name)
                            |> Function.simpleCommit
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
                , accumulator = updateAccumulatorWithBlock top_ state.accumulator
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


updateAccummulatorInStateWithBlock : SBlock -> State -> State
updateAccummulatorInStateWithBlock block state =
    { state | accumulator = updateAccumulatorWithBlock block state.accumulator }


{-|

    This function post data to the accumulator field of State as the parser
    runs its loop.  That information is used downstream by the renderer.

-}
updateAccumulatorWithBlock : SBlock -> Accumulator -> Accumulator
updateAccumulatorWithBlock sblock1 accumulator =
    case sblock1 of
        SVerbatimBlock name contentList _ ->
            if name == "mathmacro" then
                { accumulator | macroDict = LaTeX.MathMacro.makeMacroDict (String.join "\n" (List.map String.trimLeft contentList)) } |> debug3 "Accumulator (1)"

            else
                accumulator

        _ ->
            accumulator



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


debugPrefix label state =
    let
        n =
            String.fromInt state.index ++ ". "
    in
    n ++ "(" ++ label ++ ") "


debugIn label state =
    let
        _ =
            debugYellow (debugPrefix label state) state.currentLineData
    in
    state


debugOut label state =
    let
        _ =
            debugBlue (debugPrefix label state) (state.stack |> List.map Simplify.sblock)

        _ =
            debugMagenta (debugPrefix label state) (state.committed |> List.map Simplify.sblock)
    in
    state
