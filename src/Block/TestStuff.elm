module Block.TestStuff exposing (..)

import Block.Parser
import Lang.Lang exposing (Lang(..))
import Markup.API
import Markup.Simplify exposing (BlockS(..), ExprS(..))


m1 =
    """
\\begin{mathmacro}
    \\newcommand{\\bra}[0]{\\langle}
\\end{mathmacro}
"""


ami str =
    Markup.API.p MiniLaTeX str


ama str =
    Markup.API.p Markdown str


al str =
    Markup.API.p L1 str


ma str =
    Block.Parser.run Markdown 0 (String.lines str) |> .committed |> List.map Markup.Simplify.sblock


mi str =
    Block.Parser.run MiniLaTeX 0 (String.lines str) |> .committed |> List.map Markup.Simplify.sblock


ll str =
    Block.Parser.run L1 0 (String.lines str) |> .committed |> List.map Markup.Simplify.sblock
