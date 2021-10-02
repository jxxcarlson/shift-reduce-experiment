module Grammar exposing (GExpr(..))

import Error exposing (..)
import Parser.Advanced as Parser exposing ((|.), (|=), Parser)
import ParserTools


type GExpr
    = GText String
    | GMath String
    | GCode String
    | GExpr String (List GExpr)
