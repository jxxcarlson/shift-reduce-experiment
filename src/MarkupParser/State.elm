module MarkupParser.State exposing (State)

import Either exposing (Either)
import MarkupParser.AST exposing (Expr)
import MarkupParser.Token exposing (Token)


type alias State =
    { sourceText : String
    , scanPointer : Int
    , end : Int
    , stack : List (Either Token Expr)
    , committed : List Expr
    , count : Int
    }
