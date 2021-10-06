module Block.Parser exposing (..)

import Block.Library
import Block.State exposing (State)
import Dict exposing (Dict)
import Markup.Tokenizer exposing (Lang)



-- TOP LEVEL FUNCTIONS


{-| -}
runParser : Lang -> Int -> List String -> State
runParser language generation input =
    loop (initialState generation input) (nextStep language)


{-|

    nextStep depends on four top-level functions in Block.Library:

        - reduce
        - processLine
        - finalize
        - recoverFromError

-}
nextStep : Lang -> State -> Step State State
nextStep lang state =
    case List.head state.input of
        Nothing ->
            state |> Block.Library.reduce |> finalizeOrRecoverFromError

        Just line ->
            Loop (Block.Library.processLine lang line state)


finalizeOrRecoverFromError : State -> Step State State
finalizeOrRecoverFromError state =
    if List.isEmpty state.stack then
        Done (Block.Library.finalize state)

    else
        Loop (Block.Library.recoverFromError state)



-- INTIALIZERS


initialState : Int -> List String -> State
initialState generation input =
    { input = input
    , output = []
    , indent = 0
    , verbatimBlockInitialIndent = 0
    , lineNumber = 0
    , generation = generation
    , blockCount = 0
    , counter = 0
    , inVerbatimBlock = False
    , accumulator = initialAccumulator
    , stack = []
    }


initialAccumulator =
    { dict = Dict.empty }



-- HELPERS


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s f =
    case f s of
        Loop s_ ->
            loop s_ f

        Done b ->
            b
