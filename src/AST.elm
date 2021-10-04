module AST exposing (Expr(..))


type Expr
    = Text String
    | Verbatim String String
    | Expr String (List Expr)
