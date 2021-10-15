module Block.Function exposing
    ( changeStatusOfTopOfStack
    , dumpStack
    , finalize
    , finalizeBlockStatus
    , finalizeBlockStatusOfStack
    , finalizeBlockStatusOfStackTop
    , finalizeBlockStatus_
    , getStatus
    , incrementLevel
    , insertErrorMessage
    , level
    , levelOfBlock
    , levelOfCurrentBlock
    , nameOfStackTop
    , pushBlock
    , pushLineIntoBlock
    , pushLineOntoStack
    , pushLineOntoStack_
    , quantumOfIndentation
    , recoverFromError
    , reduce
    , renderErrorMessage
    , reverseCommitted
    , reverseContents
    , setBlockStatus
    , shiftBlock
    , simpleCommit
    , stackTop
    )

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.BlockTools as BlockTools
import Block.Line exposing (LineData)
import Block.State exposing (State)
import Lang.Lang exposing (Lang(..))
import Markup.Debugger exposing (debugBlue)


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


reduce : State -> State
reduce state =
    let
        finalize_ =
            reverseContents >> finalizeBlockStatus
    in
    case state.stack of
        block1 :: ((SBlock name blocks meta) as block2) :: rest ->
            if levelOfBlock block1 > levelOfBlock block2 then
                -- incorporate block1 into the block just below it in the stack
                -- then reduce again
                reduce { state | stack = SBlock name (block1 :: blocks) meta :: rest }

            else
                -- TODO: is this correct?
                reduce { state | committed = finalize_ block1 :: finalize_ block2 :: state.committed, stack = List.drop 2 state.stack }

        block :: [] ->
            -- Only one block remains on the stack, so commit it.
            -- TODO: do we need to consider error handling
            { state | committed = finalize_ block :: state.committed, stack = [] }

        _ ->
            -- TODO. This ignores many cases.  Probably wrong.
            state



-- LEVEL


levelOfCurrentBlock : State -> Int
levelOfCurrentBlock state =
    case stackTop state of
        Nothing ->
            -1

        Just block ->
            levelOfBlock block


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


quantumOfIndentation =
    3


incrementLevel : LineData -> LineData
incrementLevel lineData =
    { lineData | indent = lineData.indent + quantumOfIndentation }


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
            SParagraph strings { meta | status = finalizeBlockStatus_ (getStatus block) }

        SBlock name blocks meta ->
            SBlock name blocks { meta | status = finalizeBlockStatus_ (getStatus block) }

        SVerbatimBlock name strings meta ->
            SVerbatimBlock name strings { meta | status = finalizeBlockStatus_ (getStatus block) }

        _ ->
            block


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
