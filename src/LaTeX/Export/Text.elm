module LaTeX.Export.Text exposing (render)

import Block.Block exposing (ExprM(..))
import Dict exposing (Dict)


render : ExprM -> String
render expr =
    case expr of
        TextM string _ ->
            string

        ExprM name expressions _ ->
            renderNamedExpression name expressions

        VerbatimM name str _ ->
            if name == "math" then
                "$" ++ str ++ "$"

            else
                "\\" ++ name ++ encloseWithBraces str

        ArgM str _ ->
            encloseWithBraces (render expr)


errorText index str =
    "(" ++ String.fromInt index ++ ") not implemented: " ++ str


nameDict : Dict String String
nameDict =
    Dict.fromList
        [ ( "heading1", "section" )
        , ( "heading2", "subsection" )
        , ( "heading3", "subsubsection" )
        , ( "heading4", "subsubsubsection" )
        , ( "heading5", "subheading" )
        ]


translateName : String -> String
translateName str =
    case Dict.get str nameDict of
        Nothing ->
            str

        Just name ->
            name


renderNamedExpression : String -> List ExprM -> String
renderNamedExpression name args =
    "\\" ++ translateName name ++ renderArgs args


renderArgs : List ExprM -> String
renderArgs expressions =
    List.map (renderExpression >> encloseWithBraces) expressions |> String.join ""


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
            renderNamedExpression name expressions


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
