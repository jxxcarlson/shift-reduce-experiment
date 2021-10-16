module LaTeX.Export.Text exposing (render)

import Block.Block exposing (ExprM(..))


render : ExprM -> String
render expr =
    case expr of
        TextM string _ ->
            string

        ExprM name expressions _ ->
            renderMarked name expressions

        VerbatimM name str _ ->
            "\\" ++ name ++ encloseWithBraces str

        ArgM str _ ->
            encloseWithBraces (render expr)


errorText index str =
    "(" ++ String.fromInt index ++ ") not implemented: " ++ str


renderMarked : String -> List ExprM -> String
renderMarked name args =
    "\\" ++ name ++ renderArgs args


renderArgs : List ExprM -> String
renderArgs expressions =
    List.map renderExpression expressions |> String.join ""


renderExpression : ExprM -> String
renderExpression expr =
    case expr of
        TextM str _ ->
            str

        ArgM args _ ->
            List.map (renderExpression >> encloseWithBraces) args |> String.join ""

        VerbatimM str _ _ ->
            str

        ExprM name expressions _ ->
            renderMarked name expressions


encloseWithBraces : String -> String
encloseWithBraces str =
    "{" ++ str ++ "}"



--texmacro g s a textList =
--    macro1 (\str ->  ("\\" ++ str)) g s a textList
--
--
--texarg g s a textList =
--    macro1 (\str ->  ("{" ++ str ++ "}")) g s a textList
--
