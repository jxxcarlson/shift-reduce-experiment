module LaTeX.Export exposing (render)

import Block.Block exposing (Block(..), BlockStatus(..))
import Block.State
import Render.Settings exposing (Settings)


render : Int -> Settings -> Block.State.Accumulator -> List Block -> String
render generation settings accumulator blocks =
    "unimplemented"
