module Markup.Stack exposing (Stack, stackHasSymbol, toExprList)

import Either exposing (Either(..))
import List.Extra
import Markup.AST as AST exposing (Expr)
import Markup.Token as Token exposing (Token)
import Maybe.Extra


type alias StackItem =
    Either Token Expr


type alias Stack =
    List StackItem


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

        Right expr ->
            False


isRightExpr : StackItem -> Bool
isRightExpr stackItem =
    case stackItem of
        Left _ ->
            False

        Right _ ->
            True


isRightExprList : Stack -> Bool
isRightExprList stack =
    List.all isRightExpr stack
