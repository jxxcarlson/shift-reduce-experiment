module AST exposing (Expr(..))


type Expr
    = Text String
    | Verbatim String String
    | L1Math String
    | L1Code String
    | Expr String (List Expr)
    | TokenError String
