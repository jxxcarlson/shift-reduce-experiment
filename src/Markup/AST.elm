module Markup.AST exposing (Expr(..))

import List.Extra
import Markup.Token as Token


type Expr
    = Text String Token.Loc
    | Verbatim String String Token.Loc
    | Expr String (List Expr) Token.Loc
