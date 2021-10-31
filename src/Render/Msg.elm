module Render.Msg exposing (MarkupMsg(..))

import Markup.Meta


type MarkupMsg
    = SendMeta Markup.Meta.ExpressionMeta
    | GetPublicDocument String
