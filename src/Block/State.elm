module Block.State exposing (Accumulator, State, init)

import Block.Block exposing (SBlock)
import Block.Line
import Dict
import Lang.Lang as Lang exposing (Lang)
import Render.MathMacro



-- TYPES


type alias State =
    { input : List String
    , index : Int
    , lastIndex : Int
    , stack : List SBlock
    , currentBlock : Maybe SBlock
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


init : Int -> List String -> State
init generation input =
    { input = input
    , committed = []
    , lastIndex = List.length input
    , index = 0
    , currentLineData = { indent = 0, lineType = Block.Line.BlankLine, content = "" }
    , previousLineData = { indent = 0, lineType = Block.Line.BlankLine, content = "" }
    , currentBlock = Nothing
    , indent = 0
    , verbatimBlockInitialIndent = 0
    , generation = generation
    , blockCount = 0
    , inVerbatimBlock = False
    , accumulator = initialAccumulator
    , stack = []
    , errorMessage = Nothing
    , lang = Lang.MiniLaTeX
    }


initialAccumulator =
    { macroDict = Dict.empty }
