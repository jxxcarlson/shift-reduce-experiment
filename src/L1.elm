module L1 exposing (L1Expr(..))


type L1Expr
    = L1Text String
    | L1Math String
    | L1Code String
    | L1Expr String (List L1Expr)
