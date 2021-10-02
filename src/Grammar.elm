module Grammar exposing (GExpr(..))


type GExpr
    = GText String
    | GExpr String (List GExpr)
