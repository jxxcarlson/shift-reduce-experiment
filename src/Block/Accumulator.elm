module Block.Accumulator exposing
    ( Accumulator
    , empty
    , updateAccumulatorWithBlock
    )

import Block.Block exposing (Block(..))
import Dict
import LaTeX.MathMacro


type alias Accumulator =
    { macroDict : LaTeX.MathMacro.MathMacroDict }


empty =
    { macroDict = Dict.empty }


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
