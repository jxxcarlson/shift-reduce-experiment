module State exposing (State)

import AST exposing (Expr)
import Either exposing (Either(..))
import Token exposing (Token)


type alias State =
    { sourceText : String
    , scanPointer : Int
    , end : Int
    , stack : List (Either Token Expr)
    , committed : List Expr
    , count : Int
    }
