module Markup.API exposing (..)

import Block.Parser
import Block.State
import Markup.Block as Block exposing (Block, SBlock)
import Markup.Markup as Markup
import Markup.Tokenizer exposing (Lang(..))


parse : Lang -> Int -> List String -> { ast : List Block, accumulator : Block.State.Accumulator }
parse lang generation lines =
    let
        state =
            Block.Parser.run lang generation lines
    in
    { ast = List.map (Block.map (Markup.parseExpr lang)) state.committed, accumulator = state.accumulator }
