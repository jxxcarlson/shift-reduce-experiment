module Markup.Simplify exposing (BlockS(..), ExprS(..), SBlockS(..), TokenS(..), blocks, expressions, sblock, stack)

import Block.Block exposing (Block(..), BlockStatus(..), ExprM(..), SBlock(..))
import Either exposing (Either)
import Expression.AST exposing (Expr(..))
import Expression.Error exposing (ErrorData)
import Expression.Token as Token exposing (Token)


type ExprS
    = TextS String
    | VerbatimS String String
    | ArgS (List ExprS)
    | ExprS String (List ExprS)
    | ErrorS String


type BlockS
    = ParagraphS (List ExprS) BlockStatus
    | VerbatimBlockS String (List String) BlockStatus
    | BlockS String (List BlockS) BlockStatus
    | BErrorS String BlockStatus


type TokenS
    = TextST String
    | VerbatimST String String
    | SymbolST String
    | FunctionNameST String
    | MarkedTextST String String
    | AnnotatedTextST String String String
    | SpecialST String String
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

        Token.Special str1 str2 _ ->
            SpecialST str1 str2

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
        Paragraph exprList meta ->
            ParagraphS (List.map simplifyToExprS exprList) meta.status

        VerbatimBlock str strList _ meta ->
            VerbatimBlockS str strList meta.status

        Block name blocks_ meta ->
            BlockS name (List.map simplify blocks_) meta.status

        BError str ->
            BErrorS str BlockComplete


sblock : SBlock -> SBlockS
sblock block =
    case block of
        SParagraph strings meta ->
            SParagraphS strings meta.status

        SVerbatimBlock str strList meta ->
            SVerbatimBlockS str strList meta.status

        SBlock name blocks_ meta ->
            SBlockS name (List.map sblock blocks_) meta.status

        SError str ->
            SErrorS str


type SBlockS
    = SParagraphS (List String) BlockStatus
    | SVerbatimBlockS String (List String) BlockStatus
    | SBlockS String (List SBlockS) BlockStatus
    | SErrorS String


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

        ErrorM str ->
            ErrorS str


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

        Error str ->
            ErrorS str
