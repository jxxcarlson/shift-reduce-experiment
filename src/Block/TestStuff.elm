module Block.TestStuff exposing (al, ama, ami, ll, m1, ma, mi, qama, qami, table, table2)

import Block.Parser
import Lang.Lang exposing (Lang(..))
import Markup.API
import Markup.Simplify


m1 =
    """
\\begin{mathmacro}
    \\newcommand{\\bra}[0]{\\langle}
\\end{mathmacro}
"""


table =
    "\\begin{tabular}\n  1 & A & B \n  2 & C & D\n\\end{tabular}"


table2 =
    "\\begin{tabular}\n  1 & \\red{A} & B \n  2 & C & D\n\\end{tabular}"


table3 =
    """
\\begin{tabular}{lll}
  1 & $U \\to (V \\to U)$ & HYP \\\\
  2 & $= Int (U^c \\cup (V \\to U))$ & Def \\\\
\\end{tabular}
"""


table4 =
    """
\\begin{tabular}{lll}
  1 & $U \\to (V \\to U)$ & HYP \\\\
  2 & $= Int (U^c \\cup (V \\to U))$ & Def \\\\
\\end{tabular}
"""


qami str =
    Markup.API.q MiniLaTeX str


qama str =
    Markup.API.q Markdown str


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
