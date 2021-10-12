module Expression.State exposing (State)

import Either exposing (Either)
import Expression.AST exposing (Expr)
import Expression.Token exposing (Token)


type alias State =
    { sourceText : String
    , scanPointer : Int
    , end : Int
    , stack : List (Either Token Expr)
    , committed : List Expr
    , count : Int
    }
