module Render.Settings exposing (Settings, TitleStatus(..))


type alias Settings =
    { width : Int, titleStatus : TitleStatus }


type TitleStatus
    = TitleWithSize Int
    | HideTitle
