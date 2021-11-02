module Block.Accumulator exposing
    ( Accumulator
    , init
    , updateAccumulatorWithBlock
    )

import Block.Block exposing (Block(..))
import Dict
import LaTeX.MathMacro
import Markup.Vector as Vector exposing (Vector)


type alias Accumulator =
    { macroDict : LaTeX.MathMacro.MathMacroDict
    , sectionIndex : Vector
    }


init : Int -> Accumulator
init k =
    { macroDict = Dict.empty
    , sectionIndex = Vector.init k
    }


updateAccumulatorWithBlock : Block -> Accumulator -> Accumulator
updateAccumulatorWithBlock block accumulator =
    case block of
        VerbatimBlock name contentList _ _ ->
            if name == "mathmacro" then
                { accumulator | macroDict = LaTeX.MathMacro.makeMacroDict (String.join "\n" (List.map String.trimLeft contentList)) }

            else
                accumulator

        _ ->
            accumulator
