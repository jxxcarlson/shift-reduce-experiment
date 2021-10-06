module Markup.Block exposing (..)

--( Block(..)
--, ExpressionMeta
--, Meta
--, SBlock(..)
--, stringAtLoc
--, test
--)

import Markup.AST as AST exposing (Expr(..))
import Markup.Debugger exposing (..)
import Markup.Meta as Meta exposing (ExpressionMeta)
import Markup.Token as Token


type ExprM
    = TextM String ExpressionMeta
    | VerbatimM String String ExpressionMeta
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


dummy =
    { id = "ID", loc = { begin = { row = 0, col = 0 }, end = { row = 1, col = 5 } } }


type alias Meta =
    { begin : Int
    , end : Int
    , indent : Int
    , id : String
    }


make : String -> String -> SBlock
make id str =
    let
        lines =
            String.lines str
    in
    SParagraph lines { begin = 0, end = List.length lines, indent = 0, id = id }


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

        SVerbatimBlock name strList meta ->
            let
                exprMeta =
                    -- TODO: this is incomplete (id, last col)
                    { id = "verbatim"
                    , loc = { begin = { row = meta.begin, col = 0 }, end = { row = meta.end, col = 7 } }
                    }
            in
            VerbatimBlock name strList exprMeta meta

        SBlock name blocks meta ->
            let
                mapper : SBlock -> Block
                mapper =
                    map exprParser

                f : List SBlock -> List Block
                f =
                    List.map mapper
            in
            Block name (List.map mapper blocks) meta

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

        Verbatim name content meta ->
            VerbatimM name content (Meta.make Meta.getBlockData count meta [ content ] blockData.firstLine blockData.id)

        Expr name exprList meta ->
            ExprM name (List.map (exprToExprM count blockData) exprList) (Meta.make Meta.getBlockData count meta [] blockData.firstLine blockData.id)
