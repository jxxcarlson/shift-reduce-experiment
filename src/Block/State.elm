module Block.State exposing (..)

import Dict exposing (Dict)
import Markup.Block exposing (SBlock)
import Markup.Tokenizer exposing (Lang)



-- TYPES


type alias State =
    { input : List String
    , output : List SBlock
    , indent : Int
    , verbatimBlockInitialIndent : Int
    , lineNumber : Int
    , generation : Int
    , blockCount : Int
    , inVerbatimBlock : Bool
    , counter : Int
    , accumulator : Accumulator
    , stack : List SBlock
    }


type alias Accumulator =
    { dict : Dict String String }
