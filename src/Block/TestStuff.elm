module Block.TestStuff exposing (..)

import Block.Parser
import Lang.Lang exposing (Lang(..))
import Markup.API
import Markup.Simplify exposing (BlockS(..), ExprS(..))


m2 =
    """
## Code
"""


m1 =
    """
## Code

```
   a[1] = 1
   b[1] = 1

   c[i] = 2
   d[i] = c[i] + 1
```

## Quotation

This is a quote:

> Regular languages are rather inexpressive,
   but they work great for lexers. On the opposite 
   side of expressivity spectrum are Turing machines. 
   For them, we also have a number of meta-languages 
   (like Rust), which work great for humans.
    It’s interesting that a Turing machine is 
    equivalent to a finite state machine with 
    a pair of stacks: to get two stacks from a tape, 
    cut the tape in half where the head is. Moving 
    the head then corresponds to popping from one 
    stack and pushing to another.
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
