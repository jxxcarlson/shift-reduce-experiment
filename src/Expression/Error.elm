module Expression.Error exposing (Context(..), ErrorData, Problem(..))

import Parser.Advanced


type Problem
    = ExpectingPrefix
    | ExpectingSymbol String


type Context
    = TextExpression


type alias ErrorData =
    List (Parser.Advanced.DeadEnd Context Problem)
