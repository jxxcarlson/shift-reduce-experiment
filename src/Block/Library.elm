module Block.Library exposing
    ( classify
    , processLine
    )

import Block.Block as Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools as BlockTools
import Block.Function as Function exposing (simpleCommit)
import Block.Line exposing (LineData, LineType(..))
import Block.State exposing (State)
import Lang.Lang exposing (Lang(..))
import Lang.LineType.L1
import Lang.LineType.Markdown
import Lang.LineType.MiniLaTeX
import Markup.Debugger exposing (debugBlue, debugCyan, debugMagenta, debugRed, debugYellow)
import Markup.Simplify as Simplify
import Utility


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
        BeginBlock _ name ->
            { state | inVerbatimBlock = False }
                |> Function.setStackBottomLevelAndName state.currentLineData.indent name
                |> debugIn "BeginBlock"
                |> Function.makeBlockWithCurrentLine
                |> debugOut "BeginBlock (OUT)"

        BeginVerbatimBlock name ->
            let
                _ =
                    debugIn "BeginVerbatimBlock" state
            in
            (if Just name == Maybe.map getBlockName (Function.stackTop state) && (name == "math" || name == "code") then
                { state | inVerbatimBlock = True } |> endBlock name

             else
                { state | inVerbatimBlock = True } |> Function.setStackBottomLevelAndName state.currentLineData.indent name |> Function.makeBlockWithCurrentLine
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
            endBlock name { state | inVerbatimBlock = False }

        OrdinaryLine ->
            let
                _ =
                    debugIn "OrdinaryLine" state
            in
            if state.inVerbatimBlock && state.currentLineData.indent <= state.initialBlockIndent then
                let
                    _ =
                        debugRed "OrdinaryLine" 1
                in
                handleUnterminatedVerbatimBlock state

            else
                let
                    _ =
                        debugRed "OrdinaryLine" 3
                in
                state |> resetInVerbatimBlock |> handleOrdinaryLine

        VerbatimLine ->
            let
                _ =
                    debugIn "VerbatimLine" state
            in
            handleVerbatimLine state
                |> debugOut "VerbatimLine (OUT)"

        BlankLine ->
            let
                _ =
                    debugIn "BlankLine" state
            in
            state
                |> Utility.ifApply (isBlockUnterminatedAfterAddingLine state)
                    (handleUnterminatedBlock (Just "missing end tag"))
                |> resetInVerbatimBlock
                |> handleBlankLine
                |> debugOut "BlankLine (OUT)"

        Problem _ ->
            let
                _ =
                    debugIn "Problem" state
            in
            state



-- ORDINARY LINE


isBlockUnterminatedAfterAddingLine : State -> Bool
isBlockUnterminatedAfterAddingLine state =
    state.currentLineData.indent <= state.initialBlockIndent && (Maybe.map Block.typeOfSBlock (List.head state.stack) /= Just Block.P)



-- |> Utility.ifApply (state.currentLineData.indent <= state.initialBlockIndent && Maybe.map Block.typeOfSBlock (List.head state.stack) /= Just Block.P)
--                    (handleUnterminatedBlock (Just "missing end tag"))


handleUnterminatedBlock : Maybe String -> State -> State
handleUnterminatedBlock mStr state =
    case List.head state.stack of
        Nothing ->
            state

        Just block ->
            let
                name =
                    Maybe.map getBlockName (List.head state.stack) |> Maybe.withDefault "unnamed" |> debugRed "handleUnterminatedBlock, BLOCK NAME"

                out =
                    case mStr of
                        Nothing ->
                            "???"

                        Just str ->
                            str
            in
            if List.member name unterminatedBlockNames then
                state

            else
                state
                    |> postMessageWithBlockUnfinished out
                    |> Function.simpleCommit


unterminatedBlockNames =
    [ "item", "quotation" ]


postMessageWithBlockUnfinished : String -> State -> State
postMessageWithBlockUnfinished str =
    Function.liftBlockFunctiontoStateFunction (\b -> BlockTools.mapMeta (\m -> { m | status = BlockUnfinished str }) b)



-- |> simpleCommit


handleUnterminatedVerbatimBlock state =
    let
        _ =
            debugRed "handleUnterminatedVerbatimBlock, currentLine" state.currentLineData.content
    in
    state
        |> Utility.ifApply (state.currentLineData.content == "$") (postMessageWithBlockUnfinished "missing dollar sign?")
        |> Function.insertErrorMessage
        |> handleVerbatimLine
        |> Utility.ifApply (state.currentLineData.content /= "$") (postMessageWithBlockUnfinished "indentation?")
        |> Function.insertErrorMessage
        |> postMessageWithBlockUnfinished "indentation?"
        |> simpleCommit


resetInVerbatimBlock state =
    if state.currentLineData.indent <= state.initialBlockIndent then
        { state | inVerbatimBlock = False }

    else
        state


handleOrdinaryLine : State -> State
handleOrdinaryLine state =
    case Function.stackTop state of
        Nothing ->
            let
                _ =
                    debugRed "Nothing branch of OrdinaryLine" state
            in
            state
                |> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta "nothing to report (1)" state))
                |> debugOut "End of Nothing Branch, OrdinaryLine"

        Just top ->
            (if state.previousLineData.lineType == BlankLine then
                -- A non-blank line followed a blank one, so create a new paragraph
                state
                    |> Function.finalizeBlockStatusOfStackTop
                    |> Function.simpleCommit
                    |> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta "nothing to report (2)" state))

             else
                -- Handle the case of a non-blank line following a non-blank line.
                -- The action depends on the indentation of the current line as compared to
                -- the of the current block (top of the stack)
                let
                    _ =
                        debugRed "(i, j)" ( state.currentLineData.indent, Function.indentationOfCurrentBlock state )
                in
                case compare state.currentLineData.indent (Function.indentationOfCurrentBlock state) of
                    EQ ->
                        -- If the block on top of the stack is a paragraph, add the
                        -- current line to it.
                        if Block.typeOfSBlock top == Block.P then
                            let
                                _ =
                                    debugRed "HandleOrdinaryLine " 1
                            in
                            Function.pushLineOntoStack state.index state.currentLineData.content state

                        else
                            let
                                _ =
                                    debugRed "HandleOrdinaryLine " 2
                            in
                            -- Otherwise, commit the top block and create
                            -- a new paragraph with the current line
                            state
                                --|> Function.liftBlockFunctiontoStateFunction Function.finalizeBlockStatus
                                --|> Function.simpleCommit
                                --|> Function.pushBlock (SParagraph [ state.currentLineData.content ] (newMeta state))
                                |> addLineToStackTop
                                |> handleUnterminatedBlock (Just "indentation? (1)")

                    GT ->
                        -- The line has greater than the block on top of the stack, so add it to the block
                        -- TODO. Or should we create a new block?
                        state |> addLineToStackTop |> debugRed "TROUBLE HERE? (2) — Add ordinary line to current block (GT)"

                    LT ->
                        -- If the block on top of the stack is a verbatim block and the indentation
                        -- of the current line is less than the indentation of the block,
                        -- then signal an error but add it to the block anyway.  Otherwise, commit
                        -- the current block and create a new one.
                        -- TODO. In fact, in the else clause, we should reduce the stack, then create the block.
                        if state.initialBlockIndent == Function.indentationOfCurrentBlock state then
                            addLineToStackTop
                                { state | errorMessage = Just { red = "Below: did you forgot to indent the text?", blue = "" } }
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



-- BLANK LINE


handleBlankLine state =
    if state.previousLineData.lineType == BlankLine then
        -- ignore the repeated blank line
        state |> debugYellow "BlankLine 0"

    else
        -- TODO.  Examine with care. I think this can all be reduced to index str state or commitBlock
        let
            _ =
                debugRed "(i, j)" ( state.currentLineData.indent, Function.indentationOfCurrentBlock state + 1 )

            _ =
                debugRed "STACK TOP" (List.head state.stack)
        in
        case compare state.currentLineData.indent (Function.indentationOfCurrentBlock state + 1) of
            EQ ->
                let
                    _ =
                        debugRed "XXX BLANK" 1
                in
                -- As long as the line is of greater than or equal to
                -- the of the current verbatim block on top of the stack,
                -- stuff those lines into the block
                addLineToStackTop state |> debugYellow "BlankLine 1"

            GT ->
                let
                    _ =
                        debugRed "XXX BLANK" 2
                in
                -- as in the previous case
                -- createBlock state |> debugYellow "BlankLine 2"
                addLineToStackTop state |> debugYellow "BlankLine 1"

            LT ->
                let
                    _ =
                        debugRed "XXX BLANK" 3
                in
                -- TODO.   Can't this all be reduced to commitBlock?
                -- TODO: No!
                case Function.stackTop state of
                    Nothing ->
                        --- commitBlock state |> debugYellow "XXX BlankLine 3"
                        state |> debugYellow "XXX BlankLine 3"

                    Just _ ->
                        if Function.nameOfStackTop state == Just "math" then
                            state
                                |> Function.simpleCommit
                                |> debugYellow "XXX BlankLine 4B"

                        else if state.lang == MiniLaTeX then
                            state
                                |> Function.finalizeBlockStatusOfStackTop
                                |> Function.transformLaTeXBlockInState
                                -- TODO: an idea that doesn't work: |> ifApply (List.isEmpty state.stack) Function.simpleCommit
                                {- |> Function.simpleCommit -}
                                -- TODO: try having function reduce handle this (should be ultimate goal anyway).
                                |> Function.reduce
                                |> debugYellow "XXX BlankLine 4"

                        else
                            state
                                |> Function.finalizeBlockStatusOfStackTop
                                |> Function.simpleCommit
                                |> debugYellow "XXX BlankLine 5"



-- VERBATIM LINE


handleVerbatimLine state =
    if state.previousLineData.lineType == VerbatimLine then
        addLineToStackTop state

    else
        case compare state.currentLineData.indent (Function.indentationOfCurrentBlock state) of
            EQ ->
                let
                    _ =
                        debugRed "f, handleVerbatimLine" EQ
                in
                addLineToStackTop state

            GT ->
                let
                    _ =
                        debugRed "f, handleVerbatimLine" GT
                in
                state
                    |> addLineToStackTop
                    |> Utility.ifApply (state.currentLineData.content == "$") (postMessageWithBlockUnfinished "missing dollar sign?" >> Function.simpleCommit)

            LT ->
                let
                    _ =
                        debugRed "f, handleVerbatimLine" LT
                in
                -- TODO: is this OK?
                if state.initialBlockIndent == Function.indentationOfCurrentBlock state then
                    state
                        |> addLineToStackTop
                        |> Function.postErrorMessage
                            ""
                            "Below: you forgot to indent the block text."
                        |> Function.insertErrorMessage

                else
                    -- The indentation is too small.  Commit the block on
                    -- top of the stack and create a new block.
                    state
                        |> commitBlock
                        |> Function.makeBlockWithCurrentLine



-- CLASSIFY LINE


classify : Lang -> Bool -> Int -> String -> LineData
classify language inVerbatimBlock verbatimBlockInitialIndent str =
    let
        _ =
            ( inVerbatimBlock, verbatimBlockInitialIndent ) |> debugMagenta "(inVerbatimBlock, verbatimBlockInitialIndent)"

        lineType =
            getLineTypeParser language

        leadingSpaces =
            Block.Line.countLeadingSpaces str |> debugMagenta "leadingSpaces"

        provisionalLineType =
            lineType (String.dropLeft leadingSpaces str) |> debugMagenta "provisionalLineType"

        lineType_ =
            (if inVerbatimBlock && leadingSpaces > verbatimBlockInitialIndent then
                Block.Line.VerbatimLine

             else
                provisionalLineType
            )
                |> debugMagenta "FINAL LINE TYPE"
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


newMeta str state =
    { begin = state.index
    , end = state.index
    , status = BlockUnfinished str
    , id = String.fromInt state.generation ++ "." ++ String.fromInt state.blockCount
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
        Just _ ->
            (case Function.nameOfStackTop state of
                Nothing ->
                    -- the block is a paragraph, hence has no name
                    state |> Function.changeStatusOfTopOfStack (MismatchedTags "anonymous" name) |> Function.simpleCommit

                Just stackTopName ->
                    -- the begin and end tags match, we mark it as complete
                    -- TODO: Do we need to check the as well?
                    -- TODO: We are making an exception for item blocks which are in some sense 'autocompletd'
                    -- TODO: But making exceptions outside of 'Lang/*' is not good
                    -- if name == stackTopName || stackTopName == "item" then
                    if name == stackTopName then
                        state
                            |> Function.changeStatusOfTopOfStack BlockComplete
                            --|> Function.simpleCommit
                            |> Function.reduceStackIfTopAndBottomMatch state.currentLineData.indent name

                    else
                        -- TODO: Do we need to check the as well?
                        -- the tags don't match. We note that fact for the benefit of the renderer (or the error handler),
                        -- and we commit the block
                        state
                            |> Function.changeStatusOfTopOfStack (MismatchedTags stackTopName name)
                            -- |> Function.simpleCommit
                            |> Function.reduceStackIfTopAndBottomMatch state.currentLineData.indent name
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
            }
                |> debugCyan "commitBlock (1)"

        top :: next :: _ ->
            let
                top_ =
                    finalize_ top

                next_ =
                    finalize_ next
            in
            case compare (Function.indentationOfBlock top) (Function.indentationOfBlock next) of
                GT ->
                    Function.shiftBlock top_ state |> debugCyan "commitBlock (2)"

                EQ ->
                    { state | committed = top_ :: next_ :: state.committed, stack = List.drop 1 state.stack } |> debugMagenta "commitBlock (3)"

                LT ->
                    { state | committed = top_ :: next_ :: state.committed, stack = List.drop 1 state.stack } |> debugMagenta "commitBlock (4)"



-- HELPERS


getBlockName sblock =
    BlockTools.sblockName sblock |> Maybe.withDefault "UNNAMED"



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
            debugBlue (debugPrefix label state) (state.committed |> List.map Simplify.sblock)
    in
    state
