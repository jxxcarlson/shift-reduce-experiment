module Block.Function exposing
    ( changeStatusOfTopOfStack
    , dumpStack
    , finalize
    , finalizeBlockStatus
    , finalizeBlockStatusOfStack
    , finalizeBlockStatusOfStackTop
    , fixMarkdownBlock
    , getStatus
    , incrementLevel
    , indentationOfBlock
    , indentationOfCurrentBlock
    , insertErrorMessage
    , liftBlockFunctiontoStateFunction
    , makeBlockWithCurrentLine
    , mapStack
    , nameOfStackTop
    , postErrorMessage
    , pushBlock
    , pushLineIntoBlock
    , pushLineOntoStack
    , pushLineOntoStack_
    , quantumOfIndentation
    , recoverFromError
    , reduce
    , reduceStackIfTopAndBottomMatch
    , renderErrorMessage
    , reverseCommitted
    , reverseContents
    , setBlockStatus
    , setStackBottomLevelAndName
    , shiftBlock
    , simpleCommit
    , stackTop
    , transformLaTeXBlockInState
    )

import Block.Block as Block exposing (Block(..), BlockStatus(..), ExprM(..), SBlock(..))
import Block.BlockTools as BlockTools
import Block.Line exposing (BlockOption(..), LineData, LineType(..))
import Block.State exposing (State)
import Expression.Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Markup.Debugger exposing (..)
import Markup.Meta
import Markup.ParserTools
import Markup.Simplify as Simplify
import Parser.Advanced


makeBlockWithCurrentLine : State -> State
makeBlockWithCurrentLine state =
    case makeBlock_ state of
        Nothing ->
            state

        Just block ->
            { state | stack = block :: state.stack }


makeBlock_ : State -> Maybe SBlock
makeBlock_ state =
    case state.currentLineData.lineType of
        OrdinaryLine ->
            SParagraph [ state.currentLineData.content ] (newMeta state) |> Just

        BeginBlock RejectFirstLine mark ->
            SBlock mark [] (newMeta state) |> Just

        BeginBlock AcceptFirstLine kind ->
            SBlock (nibble state.currentLineData.content |> transformMarkdownHeading)
                [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] (newMeta state) ]
                (newMeta state)
                |> Just

        BeginBlock AcceptNibbledFirstLine kind ->
            SBlock kind
                [ SParagraph [ deleteSpaceDelimitedPrefix state.currentLineData.content ] (newMeta state) ]
                (newMeta state)
                |> Just
                |> debugRed "makBlock, AcceptNibbledFirstLine"

        BeginVerbatimBlock mark ->
            SVerbatimBlock mark [] (newMeta state) |> Just

        _ ->
            Nothing


{-| transformHeading is used for Markdown so that we can have a single, simple AST
for all markup languages handled by the system -
-}
transformMarkdownHeading : String -> String
transformMarkdownHeading str =
    case str of
        "#" ->
            "heading1"

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


nibble : String -> String
nibble str =
    case Parser.Advanced.run (Markup.ParserTools.text (\c_ -> c_ /= ' ') (\c_ -> c_ /= ' ')) str of
        Ok stringData ->
            stringData.content

        Err _ ->
            ""


newMeta state =
    { begin = state.index
    , end = state.index
    , status = BlockUnfinished "begin"
    , id = String.fromInt state.blockCount
    , indent = state.currentLineData.indent
    }


deleteSpaceDelimitedPrefix : String -> String
deleteSpaceDelimitedPrefix str =
    str
        |> String.trimLeft
        |> (\s -> String.replace (nibble s ++ " ") "" s)


postErrorMessage : String -> String -> State -> State
postErrorMessage red blue state =
    { state | errorMessage = Just { red = red, blue = blue } }


reduceStackIfTopAndBottomMatch : Int -> String -> State -> State
reduceStackIfTopAndBottomMatch level_ name state =
    if level_ == state.stackBottomLevel && name == state.stackBottomName then
        reduce state

    else
        state


setStackBottomLevelAndName : Int -> String -> State -> State
setStackBottomLevelAndName level_ name state =
    if List.isEmpty state.stack then
        { state | stackBottomLevel = level_, stackBottomName = name }

    else
        state


liftBlockFunctiontoStateFunction : (SBlock -> SBlock) -> State -> State
liftBlockFunctiontoStateFunction f state =
    { state | stack = mapStack f state.stack }


mapStack : (a -> a) -> List a -> List a
mapStack f stack =
    case List.head stack of
        Nothing ->
            stack

        Just top ->
            f top :: List.drop 1 stack


transformLaTeXBlockInState : State -> State
transformLaTeXBlockInState state =
    liftBlockFunctiontoStateFunction transformLaTeXBlock state


transformLaTeXBlock : SBlock -> SBlock
transformLaTeXBlock block =
    case block of
        SParagraph textList loc ->
            case textList of
                [] ->
                    block

                head :: rest ->
                    if String.left 5 (String.trimLeft head) == "\\item" then
                        SBlock "item" [ SParagraph (String.dropLeft 5 (String.trimLeft head) :: rest) loc ] loc

                    else
                        block

        _ ->
            block


fixMarkdownBlock : Block -> Block
fixMarkdownBlock block =
    let
        metaToExprMeta meta =
            let
                dummy =
                    Markup.Meta.dummy
            in
            { dummy | id = meta.id }
    in
    case block of
        Paragraph [ ExprM "special" [ Block.TextM "title" meta1, Block.TextM argString meta2 ] meta3 ] meta4 ->
            Paragraph [ ExprM "title" [ Block.TextM (String.trim argString) meta1 ] (metaToExprMeta meta2) ] meta4

        -- This is where Markdown items (- Foo bar baz) are made to confirm with the standard AST
        Block "item" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "item" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        -- This is where Rational Markdown numbered items (. Foo bar baz) are made to confirm with the standard AST
        Block "numberedItem" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "numberedItem" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "title" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "title" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "heading1" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "heading1" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "heading2" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "heading2" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "heading3" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "heading3" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "heading4" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "heading4" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        Block "heading5" [ Paragraph [ Block.TextM str meta ] meta2 ] meta3 ->
            Paragraph [ ExprM "heading5" [ Block.TextM (String.trim str) meta ] (metaToExprMeta meta2) ] meta3

        _ ->
            block


finalize : State -> State
finalize state =
    state
        |> finalizeBlockStatusOfStack
        |> dumpStack
        |> reverseCommitted
        |> debugBlue "FINALIZE"


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
            "[! red](" ++ msg.red ++ ")"

        --  [! blue ](" ++ msg.blue ++ ")"
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


compress : List SBlock -> List SBlock
compress blocks =
    case getBlocksOfTheSameLevel blocks of
        ( [], rest ) ->
            rest

        ( same, [] ) ->
            same

        ( same, top :: rest ) ->
            case top of
                SBlock name blocks_ meta ->
                    SBlock name (same ++ blocks_) meta :: rest

                _ ->
                    blocks


getBlocksOfTheSameLevel : List SBlock -> ( List SBlock, List SBlock )
getBlocksOfTheSameLevel blocks =
    getBlocksOfTheSameLevelHelper ( [], blocks )


getBlocksOfTheSameLevelHelper : ( List SBlock, List SBlock ) -> ( List SBlock, List SBlock )
getBlocksOfTheSameLevelHelper data =
    (case data of
        ( [], [] ) ->
            ( [], [] )

        ( first, [] ) ->
            ( first, [] )

        ( [], x :: rest ) ->
            ( [ x ], rest )

        ( block1 :: same, block2 :: rest ) ->
            if indentationOfBlock block1 == indentationOfBlock block2 then
                getBlocksOfTheSameLevelHelper ( block2 :: block1 :: same, rest )

            else
                data
    )
        |> debugSpecial "getBlocksOfTheSameLevelHelper"


reduce : State -> State
reduce state =
    let
        finalize_ =
            reverseContents >> finalizeBlockStatus
    in
    case state.stack of
        block1 :: ((SBlock name blocks meta) as block2) :: rest ->
            if indentationOfBlock block1 > indentationOfBlock block2 then
                -- TODO: ???
                -- incorporate block1 into the block just below it in the stack
                -- then reduce again
                let
                    _ =
                        debugBlue "reduce" 0
                in
                reduce { state | stack = SBlock name (block1 :: blocks) meta :: rest } |> debugOut "REDUCE 0, OUT"

            else
                let
                    _ =
                        debugBlue "reduce" 1
                in
                -- TODO: is this correct?
                -- reduce { state | committed = finalize_ block1 :: finalize_ block2 :: state.committed, stack = List.drop 2 state.stack } |> debugOut "REDUCE 1, OUT"
                state |> (\state_ -> { state_ | stack = compress state_.stack }) |> debugOut "COMPRESS IN REDUCE"

        -- state |> compress (block1 :: block2 :: []) rest |> reduce |> debugOut "REDUCE 1, OUT"
        block :: [] ->
            let
                _ =
                    debugBlue "reduce" 2
            in
            -- Only one block remains on the stack, so commit it.
            -- TODO: do we need to consider error handling
            if List.member (Block.typeOfSBlock block) [ Block.P, Block.V ] then
                if nameOfStackTop state == Just "math" then
                    { state | committed = reverseContents block :: state.committed, stack = [] } |> debugOut "REDUCE 2a, OUT"

                else
                    { state | committed = reverseContents (setBlockStatus BlockComplete block) :: state.committed, stack = [] } |> debugOut "REDUCE 2a, OUT"

            else
            --- { state | committed = reverseContents (fbefinalizeBlockStatus block) :: state.committed, stack = [] } |> debugOut "REDUCE 2b, OUT"
            if
                getStatus block == BlockComplete
            then
                { state | committed = reverseContents block :: state.committed, stack = [] } |> debugOut "REDUCE 2b, OUT"

            else
                state

        _ ->
            let
                _ =
                    debugBlue "reduce" 3
            in
            -- TODO. This ignores many cases.  Probably wrong.
            state |> debugOut "REDUCE 3, OUT"



-- LEVEL


indentationOfCurrentBlock : State -> Int
indentationOfCurrentBlock state =
    case stackTop state of
        Nothing ->
            0

        Just block ->
            indentationOfBlock block


indentationOfBlock : SBlock -> Int
indentationOfBlock block =
    case block of
        SParagraph _ meta ->
            meta.indent

        SVerbatimBlock _ _ meta ->
            meta.indent

        SBlock _ _ meta ->
            meta.indent

        SError _ ->
            0


quantumOfIndentation =
    3


incrementLevel : LineData -> LineData
incrementLevel lineData =
    { lineData | indent = lineData.indent + quantumOfIndentation }


finalizeBlockStatus : SBlock -> SBlock
finalizeBlockStatus block =
    case block of
        SParagraph strings meta ->
            SParagraph strings { meta | status = finalizeBlockStatus_ (getStatus block) }

        SBlock name blocks meta ->
            SBlock name blocks { meta | status = finalizeBlockStatus_ (getStatus block) }

        SVerbatimBlock name strings meta ->
            SVerbatimBlock name strings { meta | status = finalizeBlockStatus_ (getStatus block) }

        _ ->
            block


finalizeBlockStatus_ : BlockStatus -> BlockStatus
finalizeBlockStatus_ status =
    case status of
        BlockUnfinished _ ->
            BlockComplete

        _ ->
            status


getStatus : SBlock -> BlockStatus
getStatus block =
    BlockTools.getSBlockMeta block |> .status


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


shiftBlock : SBlock -> State -> State
shiftBlock block state =
    { state | stack = block :: state.stack }


finalizeBlockStatusOfStack : State -> State
finalizeBlockStatusOfStack state =
    { state | stack = List.map finalizeBlockStatus state.stack }


finalizeBlockStatusOfStackTop : State -> State
finalizeBlockStatusOfStackTop state =
    case List.head state.stack of
        Nothing ->
            state

        Just top ->
            { state | stack = finalizeBlockStatus top :: List.drop 1 state.stack }


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


{-| Push the given block onto the stack
-}
pushBlock : SBlock -> State -> State
pushBlock block state =
    { state | stack = block :: state.stack }


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



-- DEBUG


debugPrefix label state =
    let
        n =
            String.fromInt state.index ++ ". "
    in
    n ++ "(" ++ label ++ ") "


debugOut label state =
    let
        _ =
            debugBlue (debugPrefix label state) (state.stack |> List.map Simplify.sblock)

        _ =
            debugMagenta (debugPrefix label state) (state.committed |> List.map Simplify.sblock)
    in
    state


debugSpecial label ( firstBlocks, otherBlocks ) =
    let
        _ =
            debugMagenta (label ++ ", first") (firstBlocks |> List.map Simplify.sblock)

        _ =
            debugMagenta (label ++ ", seconds") (otherBlocks |> List.map Simplify.sblock)
    in
    ( firstBlocks, otherBlocks )
