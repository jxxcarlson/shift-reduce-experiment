module Block.State exposing (Accumulator, State, init)

import Block.Block exposing (SBlock)
import Block.Line
import Dict
import Lang.Lang exposing (Lang)
import Render.MathMacro



-- TYPES


type alias State =
    { input : List String
    , index : Int
    , lastIndex : Int
    , stack : List SBlock
    , currentLineData : Block.Line.LineData
    , previousLineData : Block.Line.LineData
    , committed : List SBlock
    , indent : Int
    , verbatimBlockInitialIndent : Int
    , generation : Int
    , blockCount : Int
    , inVerbatimBlock : Bool
    , accumulator : Accumulator
    , errorMessage : Maybe { red : String, blue : String }
    , lang : Lang
    }


type alias Accumulator =
    { macroDict : Render.MathMacro.MathMacroDict }



-- INTIALIZERS


init : Lang -> Int -> List String -> State
init lang generation input =
    { input = input
    , committed = []
    , lastIndex = List.length input
    , index = 0
    , currentLineData = { indent = 0, lineType = Block.Line.BlankLine, content = "" }
    , previousLineData = { indent = 0, lineType = Block.Line.BlankLine, content = "" }
    , indent = 0
    , verbatimBlockInitialIndent = 0
    , generation = generation
    , blockCount = 0
    , inVerbatimBlock = False
    , accumulator = initialAccumulator
    , stack = []
    , errorMessage = Nothing
    , lang = lang
    }


initialAccumulator =
    { macroDict = Dict.empty }
