module L1 exposing (Expr(..))


type Expr
    = L1Text String
    | L1Math String
    | L1Code String
    | Expr String (List Expr)
