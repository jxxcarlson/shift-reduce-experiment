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

            else if name == "code" then
                "\\verb!" ++ str ++ "!"

            else
                "\\" ++ name ++ encloseWithBraces str

        ArgM _ _ ->
            encloseWithBraces (render expr)

        ErrorM str ->
            "\\red" ++ encloseWithBraces str


nameDict : Dict String String
nameDict =
    Dict.fromList
        [ ( "heading1", "section" )
        , ( "heading2", "subsection" )
        , ( "heading3", "subsubsection" )
        , ( "heading4", "subsubsubsection" )
        , ( "heading5", "subheading" )
        , ( "date", "nothing" )
        , ( "tags", "nothing" )
        , ( "author", "nothing" )
        , ( "image", "imagecenter" ) -- needed  for compatibitlity with pdserver for now
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
    if name == "image" then
        renderImageCenter args

    else
        "\\" ++ translateName name ++ renderArgs args


renderImageCenter args =
    case List.head (List.map renderExpression args) of
        Just imageUrl ->
            "\\imagecenter" ++ encloseWithBraces (fileReferenceFromUrl imageUrl)

        Nothing ->
            "bad imagecenter reference"


fileReferenceFromUrl : String -> String
fileReferenceFromUrl url =
    case String.split "/" url |> List.reverse of
        "image.png" :: a :: _ ->
            "image/" ++ a ++ ".png"

        fname :: _ ->
            "image/" ++ fname

        [] ->
            "image/nothing.png"


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

        ErrorM str ->
            "\\red" ++ encloseWithBraces str


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
