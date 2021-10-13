module Block.Block exposing
    ( Block(..)
    , ExprM(..)
    , Meta
    , SBlock(..)
    , exprMToExpr
    , exprToExprM
    , make
    , map
    , mapMeta
    , name
    )

import Expression.AST exposing (Expr(..))
import Markup.Meta as Meta exposing (ExpressionMeta)


type ExprM
    = TextM String ExpressionMeta
    | VerbatimM String String ExpressionMeta
    | ArgM (List ExprM) ExpressionMeta
    | ExprM String (List ExprM) ExpressionMeta


type Block
    = Paragraph (List ExprM) Meta
    | VerbatimBlock String (List String) ExpressionMeta Meta
    | Block String (List Block) Meta
    | BError String


type SBlock
    = SParagraph (List String) Meta
    | SVerbatimBlock String (List String) Meta
    | SBlock String (List SBlock) Meta
    | SError String


name : SBlock -> Maybe String
name sblock =
    case sblock of
        SParagraph _ _ ->
            Nothing

        SVerbatimBlock name_ _ _ ->
            Just name_

        SBlock name_ _ _ ->
            Just name_

        SError _ ->
            Nothing


type alias Meta =
    { begin : Int
    , end : Int
    , indent : Int
    , id : String
    }


exprMToExpr : ExprM -> Expr
exprMToExpr exprM =
    case exprM of
        TextM str _ ->
            Text str { begin = 0, end = 0 }

        VerbatimM str1 str2 _ ->
            Verbatim str1 str2 { begin = 0, end = 0 }

        ArgM exprMList _ ->
            Arg (List.map exprMToExpr exprMList) { begin = 0, end = 0 }

        ExprM str exprMList _ ->
            Expr str (List.map exprMToExpr exprMList) { begin = 0, end = 0 }


mapMeta : (Meta -> Meta) -> SBlock -> SBlock
mapMeta f block =
    case block of
        SParagraph strings meta ->
            SParagraph strings (f meta)

        SVerbatimBlock name_ strings meta ->
            SVerbatimBlock name_ strings (f meta)

        SBlock name_ blocks meta ->
            SBlock name_ blocks (f meta)

        SError str ->
            SError str


make : String -> Int -> String -> SBlock
make id firstLine str =
    let
        lines =
            String.lines str
    in
    SParagraph lines { begin = firstLine, end = firstLine + List.length lines, indent = 0, id = id }


{-|

    Parse the contents of an SBlock returning a Block.

-}
map : (String -> List Expr) -> SBlock -> Block
map exprParser sblock =
    case sblock of
        SParagraph lines meta ->
            let
                blockData =
                    Meta.getBlockData lines meta.begin meta.id
            in
            Paragraph (List.indexedMap (\i expr -> exprToExprM i blockData expr) (exprParser blockData.content)) meta

        SVerbatimBlock name_ strList meta ->
            let
                exprMeta =
                    -- TODO: this is incomplete (id, last col)
                    { id = "verbatim"
                    , loc = { begin = { row = meta.begin, col = 0 }, end = { row = meta.end, col = 7 } }
                    }
            in
            VerbatimBlock name_ strList exprMeta meta

        SBlock name_ blocks meta ->
            let
                mapper : SBlock -> Block
                mapper =
                    map exprParser

                f : List SBlock -> List Block
                f =
                    List.map mapper
            in
            Block name_ (List.map mapper blocks) meta

        SError str ->
            BError str


{-|

    Using the the integer count and the information in BlockData,
    augment the information in the meta data field of the given Expr,
    producing a value of type ExprM.

-}
exprToExprM : Int -> Meta.BlockData -> Expr -> ExprM
exprToExprM count blockData expr =
    case expr of
        Text str meta ->
            TextM str (Meta.make Meta.getBlockData count meta blockData.lines blockData.firstLine blockData.id)

        Verbatim name_ content meta ->
            VerbatimM name_ content (Meta.make Meta.getBlockData count meta [ content ] blockData.firstLine blockData.id)

        Expr name_ exprList meta ->
            ExprM name_ (List.map (exprToExprM count blockData) exprList) (Meta.make Meta.getBlockData count meta [] blockData.firstLine blockData.id)

        Arg exprList meta ->
            ArgM (List.map (exprToExprM count blockData) exprList) (Meta.make Meta.getBlockData count meta [] blockData.firstLine blockData.id)
