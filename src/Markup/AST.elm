module Markup.AST exposing (Expr(..))

import Markup.Token as Token


type Expr
    = Text String Token.Loc
    | Verbatim String String Token.Loc
    | Expr String (List Expr) Token.Loc
