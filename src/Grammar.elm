module Grammar exposing (L1Expr(..))


type L1Expr
    = L1Text String
    | L1Expr String (List L1Expr)
