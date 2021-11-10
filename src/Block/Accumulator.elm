module Block.Accumulator exposing
    ( Accumulator
    , init
    , labelBlock
    , updateAccumulatorWithBlock
    )

import Block.Block exposing (Block(..), ExprM(..))
import Dict
import LaTeX.MathMacro
import Markup.Vector as Vector exposing (Vector)


type alias Accumulator =
    { macroDict : LaTeX.MathMacro.MathMacroDict
    , sectionIndex : Vector
    , theoremIndex : Vector
    , equationIndex : Vector
    }


init : Int -> Accumulator
init k =
    { macroDict = Dict.empty
    , sectionIndex = Vector.init k
    , theoremIndex = Vector.init 1
    , equationIndex = Vector.init 1
    }


{-|

    If the block is a 'Verbatim "mathmacro" block, extract a macroDict
    from its contents and store that dictionary as a field of the
    accumulator.

-}
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


{-|

    The aim of function labelBLock and its callees

        - xfolder
        - labelExpression
        - setLabel
        - labelForName

    is to set the 'label: String' field of the `ExpressionMata` component of a value
    of type ExpressionMeta.  In the case of a heading, the that field may be
    something like "3.2.0"

    This function labels expressions in a block.

-}
labelBlock : Accumulator -> Block -> { block : Block, accumulator : Accumulator }
labelBlock accumulator block =
    case block of
        Block.Block.Paragraph exprList meta ->
            List.foldl xfolder { expressions = [], accumulator = accumulator } exprList
                |> (\data -> { block = Block.Block.Paragraph (data.expressions |> List.reverse) meta, accumulator = data.accumulator })

        Block.Block.VerbatimBlock name stringList exprMeta meta ->
            if List.member name equationBlockNames then
                let
                    newEquationIndex =
                        Vector.increment 0 accumulator.equationIndex

                    newBlock =
                        Block.Block.VerbatimBlock name stringList { exprMeta | label = Vector.toString newEquationIndex } meta
                in
                { block = newBlock, accumulator = { accumulator | equationIndex = newEquationIndex } }
                -- Block.Block.Block name expressions meta

            else
                { block = block, accumulator = accumulator }

        _ ->
            { block = block, accumulator = accumulator }


equationBlockNames =
    [ "equation", "align" ]


xfolder : ExprM -> { expressions : List ExprM, accumulator : Accumulator } -> { expressions : List ExprM, accumulator : Accumulator }
xfolder expr data =
    labelExpression data.accumulator expr
        |> (\result -> { expressions = result.expr :: data.expressions, accumulator = result.accumulator })


labelExpression : Accumulator -> ExprM -> { expr : ExprM, accumulator : Accumulator }
labelExpression accumulator expr =
    case expr of
        ExprM name exprList exprMeta ->
            let
                data =
                    labelForName name accumulator
            in
            { expr = ExprM name (List.map (setLabel data.label) exprList) { exprMeta | label = data.label }, accumulator = data.accumulator }

        _ ->
            { expr = expr, accumulator = accumulator }


setLabel : String -> ExprM -> ExprM
setLabel label expr =
    case expr of
        TextM str exprMeta ->
            TextM str { exprMeta | label = label }

        VerbatimM name str exprMeta ->
            VerbatimM name str { exprMeta | label = label }

        ArgM args exprMeta ->
            ArgM args { exprMeta | label = label }

        ExprM name args exprMeta ->
            ExprM name args { exprMeta | label = label }

        ErrorM str ->
            ErrorM str


labelForName : String -> Accumulator -> { label : String, accumulator : Accumulator }
labelForName str accumulator =
    case str of
        "heading1" ->
            let
                sectionIndex =
                    Vector.increment 0 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading2" ->
            let
                sectionIndex =
                    Vector.increment 1 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading3" ->
            let
                sectionIndex =
                    Vector.increment 2 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading4" ->
            let
                sectionIndex =
                    Vector.increment 3 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        _ ->
            { label = str, accumulator = accumulator }
