module AST exposing (Expr(..))


type Expr
    = Text String
    | Verbatim String String
    | Arg (List Expr)
    | Expr String (List Expr)
    | TokenError String
