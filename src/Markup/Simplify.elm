module Markup.Simplify exposing (BlockS(..), ExprS(..), TokenS(..), blocks, expressions, stack)

import Either exposing (Either)
import Markup.AST exposing (Expr(..))
import Markup.Block exposing (Block(..), ExprM(..))
import Markup.Error exposing (Context(..), ErrorData, Problem(..))
import Markup.Token as Token exposing (Token)


type ExprS
    = TextS String
    | VerbatimS String String
    | ArgS (List ExprS)
    | ExprS String (List ExprS)


type BlockS
    = ParagraphS (List ExprS)
    | VerbatimBlockS String (List String)
    | BlockS String (List BlockS)
    | BErrorS String


type TokenS
    = TextST String
    | VerbatimST String String
    | SymbolST String
    | FunctionNameST String
    | MarkedTextST String String
    | AnnotatedTextST String String String
    | TokenErrorST ErrorData


stack : List (Either Token Expr) -> List (Either TokenS ExprS)
stack stack_ =
    List.map simplifyEitherTokenOrExpr stack_


simplifyEitherTokenOrExpr : Either Token Expr -> Either TokenS ExprS
simplifyEitherTokenOrExpr e =
    Either.mapBoth simplifyToken simplifyExprToExprS e


simplifyToken : Token -> TokenS
simplifyToken token =
    case token of
        Token.Text str _ ->
            TextST str

        Token.Verbatim str1 str2 _ ->
            VerbatimST str1 str2

        Token.Symbol str _ ->
            SymbolST str

        Token.FunctionName str _ ->
            FunctionNameST str

        Token.MarkedText name str _ ->
            MarkedTextST name str

        Token.AnnotatedText str1 str2 str3 _ ->
            AnnotatedTextST str1 str2 str3

        Token.TokenError errorData _ ->
            TokenErrorST errorData


blocks : List Block -> List BlockS
blocks blocks_ =
    List.map simplify blocks_


expressions : List Expr -> List ExprS
expressions exprList =
    List.map simplifyExprToExprS exprList


simplify : Block -> BlockS
simplify block =
    case block of
        Paragraph exprList _ ->
            ParagraphS (List.map simplifyToExprS exprList)

        VerbatimBlock str strList _ _ ->
            VerbatimBlockS str strList

        Block name blocks_ _ ->
            BlockS name (List.map simplify blocks_)

        BError str ->
            BErrorS str


simplifyToExprS : ExprM -> ExprS
simplifyToExprS expr =
    case expr of
        TextM str _ ->
            TextS str

        VerbatimM str1 str2 _ ->
            VerbatimS str1 str2

        ArgM exprList _ ->
            ArgS (List.map simplifyToExprS exprList)

        ExprM name exprList _ ->
            ExprS name (List.map simplifyToExprS exprList)


simplifyExprToExprS : Expr -> ExprS
simplifyExprToExprS expr =
    case expr of
        Text str _ ->
            TextS str

        Verbatim str1 str2 _ ->
            VerbatimS str1 str2

        Arg exprList _ ->
            ArgS (List.map simplifyExprToExprS exprList)

        Expr name exprList _ ->
            ExprS name (List.map simplifyExprToExprS exprList)
