module Expression.Stack exposing
    ( Stack
    , dump
    , isFunctionName
    , isStackReducible
    , stackHasSymbol
    , symbolToString
    , toExprList
    )

import Either exposing (Either(..))
import Expression.AST as AST exposing (Expr)
import Expression.Token as Token exposing (Token(..))
import Lang.Lang exposing (Lang(..))
import Maybe.Extra


type alias StackItem =
    Either Token Expr


type alias Stack =
    List StackItem


type StackCharacter
    = L
    | R


type alias StackCharacteristic =
    List StackCharacter


stackCharacteristic : Stack -> StackCharacteristic
stackCharacteristic stack =
    List.map characteristicSymbol stack |> Maybe.Extra.values


updateStatus : StackCharacter -> Int -> Int
updateStatus char k =
    case char of
        L ->
            k - 1

        R ->
            k + 1


isStackReducible : Stack -> Bool
isStackReducible stack =
    stack |> stackCharacteristic |> isReducible


isReducible : StackCharacteristic -> Bool
isReducible sc =
    List.foldl (\c acc -> updateStatus c acc) 0 sc == 0


characteristicSymbol : StackItem -> Maybe StackCharacter
characteristicSymbol item =
    case item of
        Right _ ->
            Nothing

        Left token ->
            case token of
                Symbol "(" _ ->
                    Just L

                Symbol ")" _ ->
                    Just R

                _ ->
                    Nothing


symbolToString : StackItem -> Maybe String
symbolToString item =
    case item of
        Left token ->
            Token.symbolToString token

        Right _ ->
            Nothing


dump : Lang -> Stack -> String
dump lang stack =
    List.map (dumpItem lang) stack |> List.reverse |> String.join "" |> String.trim


dumpItem : Lang -> StackItem -> String
dumpItem lang stackItem =
    case stackItem of
        Left token ->
            Token.stringValue token

        Right expr ->
            case lang of
                MiniLaTeX ->
                    AST.miniLaTeXStringValue expr

                Markdown ->
                    AST.markdownStringValue expr

                L1 ->
                    AST.markdownStringValue expr


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
