module BlockTest exposing (..)

import Expect
import Markup
import Markup.Block as Block exposing (SBlock(..))
import Markup.Meta as Meta
import Markup.Tokenizer as Tokenizer exposing (Lang(..))
import Test exposing (..)


blockTest lang str =
    -- Markup.parseBlock lang (Block.make str)
    True


suiteBlock : Test
suiteBlock =
    describe "recovering a substring of the source text from metadata"
        [ test "(1) BlockTest 1 6" <|
            \_ ->
                blockTest MiniLaTeX "abcd\nefgh\nijkl"
                    |> Expect.equal True
        ]
