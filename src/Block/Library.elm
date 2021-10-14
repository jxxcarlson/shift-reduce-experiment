module Block.Library exposing
    ( classify
    , finalize
    , processLine
    , recoverFromError
    , reduce
    )

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools as BlockTools
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


finalize : State -> State
finalize state =
    state |> dumpStack |> reverseCommitted |> debugMagenta "FINALIZE"


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
            case ( msg.red, msg.blue ) of
                ( "", "" ) ->
                    ""

                ( red, "" ) ->
                    "\\red{" ++ red ++ "}"

                ( "", blue ) ->
                    "\\skip{10} \\blue{" ++ blue ++ "}"

                ( red, blue ) ->
                    "\\red{" ++ red ++ "} \\skip{10} \\blue{" ++ blue ++ "}"


recoverFromError : State -> State
recoverFromError state =
    { state | stack = [] } |> debugBlue "recoverFromError "


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
            if Just mark == Maybe.map getBlockName (stackTop state) && (mark == "math" || mark == "code") then
                state |> endBlock mark

            else
                createBlock state |> deBUG4 "BeginVerbatimBlock (OUT)"

        EndBlock name ->
            endBlock name state

        EndVerbatimBlock name ->
            endBlock name state

        OrdinaryLine ->
            (let
                _ =
                    deBUG4 "OrdinaryLine (IN)" state
             in
             if state.previousLineData.lineType == BlankLine then
                createBlock state |> debugRed "TROUBLE HERE? (7)"

             else
                case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
                    EQ ->
                        case stackTop state of
                            Nothing ->
                                state |> debugRed "TROUBLE HERE? (6)"

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
                        if state.verbatimBlockInitialIndent == levelOfCurrentBlock state then
                            addLineToCurrentBlock { state | errorMessage = Just { red = "Below: you forgot to indent the math text. This is needed for all blocks.  Also, remember the trailing dollar signs", blue = "" } }
                                |> insertErrorMessage

                        else
                            state |> commitBlock |> createBlock |> debugRed "TROUBLE HERE? (1)"
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
                case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
                    EQ ->
                        addLineToCurrentBlock state

                    GT ->
                        addLineToCurrentBlock state

                    LT ->
                        if state.verbatimBlockInitialIndent == levelOfCurrentBlock state then
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
                case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
                    EQ ->
                        let
                            _ =
                                debugYellow "BlankLine" 1
                        in
                        addLineToCurrentBlock state

                    GT ->
                        let
                            _ =
                                debugYellow "BlankLine" 2
                        in
                        createBlock state

                    LT ->
                        case stackTop state of
                            Nothing ->
                                let
                                    _ =
                                        debugYellow "BlankLine" 3
                                in
                                commitBlock state

                            Just block ->
                                if state.lang == MiniLaTeX then
                                    let
                                        _ =
                                            debugYellow "BlankLine" 4
                                    in
                                    state
                                        |> commitBlock

                                else
                                    let
                                        _ =
                                            debugYellow "BlankLine" 5
                                    in
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
            debugYellow "EndBlock, name" name

        _ =
            deBUG4 "EndBlock (IN)" state
     in
     case nameOfStackTop state of
        Nothing ->
            state |> changeStatusOfTopOfStack (MismatchedTags "anonymous" name) |> simpleCommit

        Just stackTopName ->
            if name == stackTopName then
                state |> changeStatusOfTopOfStack BlockComplete |> simpleCommit

            else
                state |> changeStatusOfTopOfStack (MismatchedTags stackTopName name) |> simpleCommit
    )
        |> deBUG4 "EndBlock (OUT)"


deBUG4 label state =
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


deBUG1 label state =
    let
        n =
            String.fromInt state.index ++ ". "

        _ =
            debugMagenta (n ++ label ++ ": line") state.currentLineData

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


reduce : State -> State
reduce state =
    case state.stack of
        block1 :: ((SBlock name blocks meta) as block2) :: rest ->
            if levelOfBlock block1 > levelOfBlock block2 then
                -- incorporate block1 into the block just below it in the stack
                -- then reduce again
                reduce { state | stack = SBlock name (block1 :: blocks) meta :: rest }

            else
                -- TODO: is this correct?
                reduce { state | committed = block1 :: block2 :: state.committed, stack = List.drop 2 state.stack }

        block :: [] ->
            -- Only one block remains on the stack, so commit it.
            -- TODO: do we need to consider error handling
            { state | committed = reverseContents block :: state.committed, stack = [] }

        _ ->
            -- TODO. This ignores many cases.  Probably wrong.
            state


createBlock : State -> State
createBlock state =
    state |> createBlockPhase1 |> createBlockPhase2


createBlockPhase1 : State -> State
createBlockPhase1 state =
    -- Determine whether we need to reduce the stack, pushing something onto committed
    case compare (level state.currentLineData.indent) (levelOfCurrentBlock state) of
        LT ->
            case stackTop state of
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
            case stackTop state of
                Nothing ->
                    commitBlock state

                Just _ ->
                    let
                        -- TODO: think about this
                        errorMessage_ =
                            debugBlue "createBlockPhase1 (EQ)" (Just { red = "You need to terminate this block (2)", blue = "??2" })
                    in
                    simpleCommit state |> debugRed "TROUBLE HERE? (3)"

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
            pushBlock newBlock state

        BeginBlock RejectFirstLine mark ->
            let
                newBlock =
                    SBlock mark [] { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            pushBlock newBlock state

        BeginBlock AcceptFirstLine _ ->
            let
                newBlock =
                    SBlock (nibble state.currentLineData.content |> transformHeading)
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockStarted, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            pushBlock newBlock state

        BeginBlock AcceptNibbledFirstLine kind ->
            let
                newBlock =
                    SBlock kind
                        [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] { status = BlockStarted, begin = state.index, end = state.index, id = String.fromInt state.blockCount, indent = state.currentLineData.indent } ]
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            pushBlock newBlock state

        BeginVerbatimBlock mark ->
            let
                newBlock =
                    SVerbatimBlock mark
                        []
                        { begin = state.index, end = state.index, status = BlockStarted, id = String.fromInt state.blockCount, indent = state.currentLineData.indent }
            in
            { state
                | currentLineData = incrementLevel state.currentLineData -- do this because a block expects subsequent lines to be indented
                , inVerbatimBlock = True
                , verbatimBlockInitialIndent = state.currentLineData.indent + quantumOfIndentation -- account for indentation of succeeding lines
                , blockCount = state.blockCount + 1
            }
                |> pushBlock newBlock

        _ ->
            state
    )
        |> debugCyan "createBlock "


commitBlock : State -> State
commitBlock state =
    state |> insertErrorMessage |> commitBlock_


commitBlock_ : State -> State
commitBlock_ state =
    let
        finalize_ =
            finalizeBlockStatus >> reverseContents
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
            case compare (levelOfBlock top) (levelOfBlock next) of
                GT ->
                    shiftBlock top_ state |> debugCyan "commitBlock (2)"

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


shiftBlock : SBlock -> State -> State
shiftBlock block state =
    { state | stack = block :: state.stack }


addLineToCurrentBlock : State -> State
addLineToCurrentBlock state =
    (case stackTop state of
        Nothing ->
            state

        Just (SParagraph lines meta) ->
            pushLineOntoStack state.index state.currentLineData.content state

        Just (SBlock mark blocks meta) ->
            let
                top =
                    SBlock mark (addLineToBlocks state.index state.currentLineData blocks) { meta | end = state.index }
            in
            { state | stack = top :: List.drop 1 state.stack }

        Just (SVerbatimBlock mark lines meta) ->
            pushLineOntoStack state.index state.currentLineData.content state

        _ ->
            state
    )
        |> deBUG1 "addLineToCurrentBlock"



-- HELPERS


finalizeBlockStatus_ : BlockStatus -> BlockStatus
finalizeBlockStatus_ status =
    if status == BlockStarted then
        BlockComplete

    else
        status


finalizeBlockStatus : SBlock -> SBlock
finalizeBlockStatus block =
    case block of
        SParagraph strings meta ->
            SParagraph strings { meta | status = finalizeBlockStatus_ (BlockTools.getSBlockMeta block |> .status) }

        SBlock name blocks meta ->
            SBlock name blocks { meta | status = finalizeBlockStatus_ (BlockTools.getSBlockMeta block |> .status) }

        SVerbatimBlock name strings meta ->
            SVerbatimBlock name strings { meta | status = finalizeBlockStatus_ (BlockTools.getSBlockMeta block |> .status) }

        _ ->
            block


reverseCommitted : State -> State
reverseCommitted state =
    { state | committed = List.reverse state.committed }


dumpStack : State -> State
dumpStack state =
    { state | committed = state.stack ++ state.committed, stack = [] }


pushLineOntoStack : Int -> String -> State -> State
pushLineOntoStack index str state =
    { state | stack = pushLineOntoStack_ index str state.stack }


pushLineOntoStack_ : Int -> String -> List SBlock -> List SBlock
pushLineOntoStack_ index str stack =
    case List.head stack of
        Nothing ->
            stack

        Just top ->
            pushLineIntoBlock index str top :: List.drop 1 stack


pushLineIntoBlock : Int -> String -> SBlock -> SBlock
pushLineIntoBlock index str block =
    case block of
        SParagraph strings meta ->
            SParagraph (str :: strings) { meta | end = index }

        SVerbatimBlock name strings meta ->
            SVerbatimBlock name (str :: strings) { meta | end = index }

        _ ->
            block


pushBlock : SBlock -> State -> State
pushBlock block state =
    { state | stack = block :: state.stack, blockCount = state.blockCount + 1 }


changeStatusOfTopOfStack : BlockStatus -> State -> State
changeStatusOfTopOfStack status state =
    case stackTop state of
        Nothing ->
            state

        Just block ->
            { state | stack = setBlockStatus status block :: List.drop 1 state.stack }


setBlockStatus : BlockStatus -> SBlock -> SBlock
setBlockStatus status block =
    BlockTools.mapMeta (\meta -> { meta | status = status }) block


simpleCommit : State -> State
simpleCommit state =
    -- Assume that the status of the block on top of the stack has already been set
    case List.head state.stack of
        Nothing ->
            state

        Just block ->
            { state | committed = reverseContents block :: state.committed, stack = List.drop 1 state.stack }


stackTop : State -> Maybe SBlock
stackTop state =
    List.head state.stack


nameOfStackTop : State -> Maybe String
nameOfStackTop state =
    Maybe.andThen BlockTools.sblockName (stackTop state)


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
            (-- if inVerbatimBlock && provisionalLineType == Block..BlankLine then
             if inVerbatimBlock && leadingSpaces >= (verbatimBlockInitialIndent |> debugCyan "verbatimBlockInitialIndent") then
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
    case stackTop state of
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
