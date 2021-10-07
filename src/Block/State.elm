module Block.State exposing (..)

import Block.Line
import Dict exposing (Dict)
import Markup.Block exposing (SBlock)
import Markup.Tokenizer exposing (Lang)



-- TYPES


type alias State =
    { input : List String
    , index : Int
    , lastIndex : Int
    , stack : List SBlock
    , currentBlock : SBlock
    , currentLine : String
    , currentLineData : Block.Line.LineData
    , committed : List SBlock
    , indent : Int
    , verbatimBlockInitialIndent : Int
    , generation : Int
    , blockCount : Int
    , inVerbatimBlock : Bool
    , accumulator : Accumulator
    }


type alias Accumulator =
    { dict : Dict String String }



-- INTIALIZERS


init : Int -> List String -> State
init generation input =
    { input = input
    , committed = []
    , lastIndex = List.length input
    , index = 0
    , currentLine = ""
    , currentLineData = { indent = 0, lineType = Block.Line.BlankLine, content = "" }
    , currentBlock = Markup.Block.SSystem "initialBlock"
    , indent = 0
    , verbatimBlockInitialIndent = 0
    , generation = generation
    , blockCount = 0
    , inVerbatimBlock = False
    , accumulator = initialAccumulator
    , stack = []
    }


initialAccumulator =
    { dict = Dict.empty }