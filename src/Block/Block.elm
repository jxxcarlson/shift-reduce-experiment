module Block.Block exposing
    ( Block(..)
    , BlockStatus(..)
    , ExprM(..)
    , Meta
    , SBlock(..)
    , dummyMeta
    )

import Markup.Meta as Meta exposing (ExpressionMeta)


type alias Meta =
    { begin : Int
    , end : Int
    , indent : Int
    , id : String
    , status : BlockStatus
    }


dummyMeta =
    { begin = 0, end = 0, indent = 0, id = "ID", status = BlockComplete }


type BlockStatus
    = BlockIncomplete String
    | BlockComplete


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
