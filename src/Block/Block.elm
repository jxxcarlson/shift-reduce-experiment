module Block.Block exposing
    ( Block(..)
    , BlockStatus(..)
    , BlockType(..)
    , ExprM(..)
    , Meta
    , SBlock(..)
    , dummyMeta
    , typeOfSBlock
    )

import Markup.Meta exposing (ExpressionMeta)


type alias Meta =
    { begin : Int
    , end : Int
    , indent : Int
    , id : String
    , status : BlockStatus
    , label : String
    }


dummyMeta : Meta
dummyMeta =
    { begin = 0, end = 0, indent = 0, id = "ID", status = BlockComplete, label = "" }


type Block
    = Paragraph (List ExprM) Meta
    | VerbatimBlock String (List String) ExpressionMeta Meta
    | Block String (List Block) Meta
    | BError String


type BlockStatus
    = BlockUnfinished String
    | MismatchedTags String String
    | BlockUnimplemented
    | BlockComplete


type BlockType
    = P
    | V
    | B
    | E


type ExprM
    = TextM String ExpressionMeta
    | VerbatimM String String ExpressionMeta
    | ArgM (List ExprM) ExpressionMeta
    | ExprM String (List ExprM) ExpressionMeta
    | ErrorM String


type SBlock
    = SParagraph (List String) Meta
    | SVerbatimBlock String (List String) Meta
    | SBlock String (List SBlock) Meta
    | SError String


typeOfSBlock : SBlock -> BlockType
typeOfSBlock block =
    case block of
        SParagraph _ _ ->
            P

        SVerbatimBlock _ _ _ ->
            V

        SBlock _ _ _ ->
            B

        SError _ ->
            E
