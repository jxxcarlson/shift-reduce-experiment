module Block.TestStuff exposing (..)

import Block.Parser
import Lang.Lang exposing (Lang(..))
import Markup.API
import Markup.Simplify exposing (BlockS(..), ExprS(..))


m1 =
    """
- Put gas in the car

- Drive to Las Vegas
"""


m2 =
    """
. Put gas in the car

. Drive to Las Vegas
"""


t1 =
    "\\begin{itemize}\n   \n   \\item Apples\n   \n   \\item Oranges\n   \n\\end{itemize}"


t2 =
    "\\begin{enumerate}\n   \n   \\item Apples\n   \n   \\item Oranges\n   \n\\end{enumerate}"


mi3 =
    """
\\begin{item}
   foo
   bar
\\end{item}
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
