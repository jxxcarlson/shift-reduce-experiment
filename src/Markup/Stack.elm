module Markup.Stack exposing (reduce1)

import Either exposing (Either(..))
import Markup.AST as AST exposing (Expr)
import Markup.Token as Token exposing (Token)


type alias StackItem =
    Either Token Expr


type alias Stack =
    List StackItem


isSymbol : StackItem -> Bool
isSymbol stackItem =
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


reduce1 : Stack -> Stack
reduce1 stack =
    case stack of
        (Left (Token.Symbol "[" loc1)) :: rest ->
            case List.reverse stack of
                (Left (Token.Symbol "]" loc2)) :: ((Left (Token.FunctionName name loc3)) :: rest2) ->
                    let
                        interior =
                            List.take (List.length stack - 1) stack |> List.drop 2
                    in
                    if isRightExprList interior then
                        Right (AST.Expr name (List.map Either.toList interior |> List.concat) loc3) :: []

                    else
                        -- TODO: no recursion! BAD!!
                        stack

                -- TODO: loc3 is totally bogus
                _ ->
                    stack

        _ ->
            stack
