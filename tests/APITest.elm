module APITest exposing (parseMiniLaTeX, suiteAPITestMiniLaTeX)

import Block.Accumulator exposing (Accumulator)
import Block.Block exposing (Block(..), BlockStatus(..), ExprM(..))
import Block.State
import Dict
import Expect
import Lang.Lang exposing (Lang(..))
import Markup.API as API
import Test exposing (..)


parseMiniLaTeX : String -> { ast : List Block, accumulator : Accumulator }
parseMiniLaTeX str =
    API.parse MiniLaTeX 0 (String.lines str)


suiteAPITestMiniLaTeX : Test
suiteAPITestMiniLaTeX =
    describe "parsing MiniLaTeX to List Block + Accumulator"
        [ test "(1) " <|
            \_ ->
                parseMiniLaTeX "This \\italic{good} stuff: $x^2$\n\n\\begin{theorem}\n   Pythagoras sez: $a^2 + b^2 = c^2$"
                    |> Expect.equal
                        { accumulator = { equationIndex = { content = [ 0 ], size = 1 }, macroDict = Dict.fromList [], sectionIndex = { content = [ 0, 0, 0, 0 ], size = 4 }, theoremIndex = { content = [ 1 ], size = 1 } }
                        , ast =
                            [ Paragraph
                                [ TextM "This " { id = "0.1.0", label = "", loc = { begin = { col = 0, row = 0 }, end = { col = 4, row = 0 } } }
                                , ExprM "italic" [ TextM "good" { id = "0.1.1", label = "italic", loc = { begin = { col = 13, row = 0 }, end = { col = 16, row = 0 } } } ] { id = "0.1.1", label = "italic", loc = { begin = { col = 5, row = -1 }, end = { col = 17, row = -1 } } }
                                , TextM " stuff: " { id = "0.1.2", label = "", loc = { begin = { col = 18, row = 0 }, end = { col = 25, row = 0 } } }
                                , VerbatimM "math" "x^2" { id = "0.1.3", label = "", loc = { begin = { col = 26, row = 0 }, end = { col = 30, row = 0 } } }
                                , TextM "\n" { id = "0.1.4", label = "", loc = { begin = { col = 31, row = 0 }, end = { col = 31, row = 0 } } }
                                ]
                                { begin = 0, end = 0, id = "0.1", indent = 0, label = "", status = BlockComplete }
                            , Block "theorem"
                                [ Paragraph
                                    [ TextM "   Pythagoras sez: " { id = "3.0", label = "", loc = { begin = { col = 0, row = 3 }, end = { col = 18, row = 3 } } }
                                    , VerbatimM "math" "a^2 + b^2 = c^2" { id = "3.1", label = "", loc = { begin = { col = 19, row = 3 }, end = { col = 35, row = 3 } } }
                                    , TextM "\n" { id = "3.2", label = "", loc = { begin = { col = 36, row = 3 }, end = { col = 36, row = 3 } } }
                                    ]
                                    { begin = 3, end = 3, id = "3", indent = 3, label = "", status = BlockComplete }
                                ]
                                { begin = 2, end = 3, id = "3", indent = 0, label = "1", status = BlockUnfinished "begin" }
                            ]
                        }
        ]
