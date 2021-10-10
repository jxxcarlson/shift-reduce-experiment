module Markup.Stack exposing (Stack, isFunctionName, stackHasSymbol, toExprList)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr)
import Markup.Token as Token exposing (Token(..))
import Maybe.Extra


type alias StackItem =
    Either Token Expr


type alias Stack =
    List StackItem


isFunctionName : StackItem -> Bool
isFunctionName stackItem =
    case stackItem of
        Left (FunctionName _ _) ->
            True

        _ ->
            False


toExprList : Stack -> Maybe (List Expr)
toExprList stack =
    List.map stackItemToExpr stack |> Maybe.Extra.combine


stackItemToExpr : StackItem -> Maybe Expr
stackItemToExpr stackItem =
    case stackItem of
        Right expr ->
            Just expr

        Left (Token.Text str loc) ->
            Just (AST.Text str loc)

        _ ->
            Nothing


stackHasSymbol : Stack -> Bool
stackHasSymbol stack =
    List.any hasSymbol stack


hasSymbol : StackItem -> Bool
hasSymbol stackItem =
    case stackItem of
        Left token ->
            Token.isSymbol token

        Right _ ->
            False
