module Markup.State exposing (State)

import Either exposing (Either)
import Markup.AST exposing (Expr)
import Markup.Token exposing (Token)


type alias State =
    { sourceText : String
    , scanPointer : Int
    , end : Int
    , stack : List (Either Token Expr)
    , committed : List Expr
    , count : Int
    }
