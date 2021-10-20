module LaTeX.Export.Block exposing (render)

import Block.Block exposing (Block(..), BlockStatus(..))
import Element exposing (..)
import Element.Background as Background
import LaTeX.Export.Data
import LaTeX.Export.Text


render : String -> List Block -> String
render title blocks =
    LaTeX.Export.Data.preamble title
        ++ (List.map renderBlock (excludeTitle blocks) |> String.join "\n\n")
        ++ "\n\\end{document}"


excludeTitle : List Block -> List Block
excludeTitle blocks =
    List.filter isNotTitleBlock blocks


isNotTitleBlock block =
    case block of
        Paragraph ((Block.Block.ExprM name _ _) :: rest2) _ ->
            name /= "title"

        _ ->
            True


renderVerbatimEnvironment name body =
    if name == "mathmacro" then
        "%% User's macros\n" ++ body ++ "\n"

    else if name == "math" then
        "$$\n" ++ body ++ "\n$$"

    else
        "\\begin{" ++ name ++ "}\n" ++ body ++ "\n\\end{" ++ name ++ "}\n"


renderEnvironment name blocks =
    renderVerbatimEnvironment name (List.map renderBlock blocks |> String.join "\n")


renderBlock : Block -> String
renderBlock block =
    case block of
        Paragraph textList _ ->
            List.map LaTeX.Export.Text.render textList |> String.join " "

        VerbatimBlock name lines _ meta ->
            if meta.status /= BlockComplete then
                renderLinesIncomplete name meta.status lines

            else
                renderVerbatimEnvironment name (String.join "\n" lines)

        Block name blocks meta ->
            if meta.status /= BlockComplete then
                renderBlocksIncomplete name meta.status blocks

            else
                renderEnvironment name blocks

        BError desc ->
            "Error: " ++ desc


renderBlocksIncomplete : String -> BlockStatus -> List Block -> String
renderBlocksIncomplete name status blocks =
    statusToString status ++ renderEnvironment name blocks


renderLinesIncomplete : String -> BlockStatus -> List String -> String
renderLinesIncomplete name status lines =
    statusToString status ++ renderVerbatimEnvironment name (String.join "\n" lines)


statusToString : BlockStatus -> String
statusToString status =
    case status of
        BlockUnfinished ->
            "Error: environment is missing a matching tag at the end"

        MismatchedTags tag1 tag2 ->
            "Error: the tags " ++ tag1 ++ " and " ++ tag2 ++ " are not the same."

        BlockUnimplemented ->
            "Error: unimplemented environment"

        BlockComplete ->
            ""


error str =
    paragraph [ Background.color (rgb255 250 217 215) ] [ text str ]


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"
