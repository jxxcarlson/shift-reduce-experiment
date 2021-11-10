module Expression.ASTTools exposing
    ( FilterType(..)
    , args2
    , args2M
    , exprListToStringList
    , filter
    , filterBlockByName
    , filterExpressions
    , filterStrictBlock
    , findIdsMatchingText
    , getHeadings
    , getItem
    , getText
    , listExprMToString
    , reverseContents
    , stringContentOfNamedBlock
    , stringValue
    , stringValueOfList
    , textToString
    )

-- import Block.Block exposing (Block(..), ExprM(..))

import Block.Block exposing (Block(..), ExprM(..))
import Markup.Meta as Meta
import Maybe.Extra


matchName : String -> ExprM -> Bool
matchName str expr =
    case expr of
        VerbatimM name _ _ ->
            name == str

        ExprM name _ _ ->
            name == str

        _ ->
            False


filterExpressions : String -> List ExprM -> List ExprM
filterExpressions key expressions =
    List.filter (matchName key) expressions


{-| [Text "a b c d"] -> [Text "a b c", Text "d"]
-}
args2M : List ExprM -> List ExprM
args2M exprList =
    let
        args =
            args2 exprList
    in
    [ TextM args.first Meta.dummy, TextM args.last Meta.dummy ]


args2 : List ExprM -> { first : String, last : String }
args2 exprList =
    let
        args =
            exprListToStringList exprList |> String.join " " |> String.words

        n =
            List.length args

        first =
            List.take (n - 1) args |> String.join " "

        last =
            List.drop (n - 1) args |> String.join " "
    in
    { first = first, last = last }


exprListToStringList : List ExprM -> List String
exprListToStringList exprList =
    List.map getText exprList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


getText : ExprM -> Maybe String
getText text =
    case text of
        TextM str _ ->
            Just str

        VerbatimM _ str _ ->
            Just (String.replace "`" "" str)

        ExprM _ expressions _ ->
            List.map getText expressions |> Maybe.Extra.values |> String.join " " |> Just

        _ ->
            Nothing


getName : ExprM -> Maybe String
getName text =
    case text of
        ExprM str _ _ ->
            Just str

        _ ->
            Nothing


listExprMToString : List ExprM -> String
listExprMToString list =
    List.map exprMToString list |> String.join "\n"


exprMToString : ExprM -> String
exprMToString text =
    case text of
        TextM string _ ->
            string

        ExprM _ textList _ ->
            List.map exprMToString textList |> String.join "\n"

        ArgM textList _ ->
            List.map exprMToString textList |> String.join "\n"

        VerbatimM _ str _ ->
            str

        ErrorM str ->
            str


reverseContents : ExprM -> ExprM
reverseContents expr =
    case expr of
        ExprM name textList meta ->
            ExprM name (List.reverse textList) meta

        ArgM textList meta ->
            ArgM (List.reverse textList) meta

        _ ->
            expr


getItem : String -> List Block -> Maybe String
getItem name blocks =
    let
        result =
            filterStrictBlock Equality name blocks
    in
    if result == "" then
        Nothing

    else
        Just result


getHeadings : List Block -> List ExprM
getHeadings blocks =
    filter Contains "heading" blocks


filter : FilterType -> String -> List Block -> List ExprM
filter filterType key blocks =
    List.map (filter_ filterType key) blocks |> List.concat


findIdsMatchingText : String -> List Block -> List String
findIdsMatchingText str blocks =
    List.filter (blockContains str) blocks |> List.map blockToId


blockContains : String -> Block -> Bool
blockContains key block =
    case block of
        Paragraph exprs _ ->
            List.any (exprContains key) exprs

        VerbatimBlock _ strings _ _ ->
            List.any (String.contains key) strings

        Block _ blocks _ ->
            List.any (blockContains key) blocks

        BError _ ->
            False


exprContains : String -> ExprM -> Bool
exprContains key expr =
    case expr of
        TextM str _ ->
            String.contains key str

        VerbatimM _ str _ ->
            String.contains key str

        ArgM exprs _ ->
            List.any (exprContains key) exprs

        ExprM _ exprs _ ->
            List.any (exprContains key) exprs

        ErrorM _ ->
            False


blockToId : Block -> String
blockToId block =
    case block of
        Paragraph _ meta ->
            meta.id

        VerbatimBlock _ _ _ meta ->
            meta.id

        Block _ _ meta ->
            meta.id

        BError _ ->
            "(error)"


filterStrictBlock : FilterType -> String -> List Block -> String
filterStrictBlock filterType key blocks =
    List.map (filterStrictBlock_ filterType key) blocks |> String.join ""


filter_ : FilterType -> String -> Block -> List ExprM
filter_ filterType key block =
    case block of
        Paragraph textList _ ->
            case filterType of
                Equality ->
                    List.filter (\t -> Maybe.map (\x -> x == key) (getName t) == Just True) textList
                        ++ List.filter (\t -> Maybe.map (\x -> x == "special") (getName t) == Just True) textList

                Contains ->
                    List.filter (\t -> Maybe.map (String.contains key) (getName t) == Just True) textList

        Block name blocks _ ->
            case filterType of
                Equality ->
                    if key == name then
                        [ ExprM name (List.map extractContents blocks |> List.concat) { id = "??", loc = { begin = { row = 0, col = 0 }, end = { row = 0, col = 0 } }, label = "" } ]

                    else
                        []

                Contains ->
                    if String.contains key name then
                        [ ExprM name (List.map extractContents blocks |> List.concat) { id = "??", loc = { begin = { row = 0, col = 0 }, end = { row = 0, col = 0 } }, label = "" } ]

                    else
                        []

        _ ->
            []


extractContents : Block -> List ExprM
extractContents block =
    case block of
        Paragraph contents _ ->
            contents

        _ ->
            []


type FilterType
    = Equality
    | Contains


filterStrictBlock_ : FilterType -> String -> Block -> String
filterStrictBlock_ filterType key block =
    case block of
        Paragraph textList _ ->
            case filterType of
                Equality ->
                    List.filter (\t -> Just key == getName t) textList |> listExprMToString

                Contains ->
                    List.filter (\t -> Maybe.map2 String.contains (Just key) (getName t) == Just True) textList |> listExprMToString

        Block name blocks _ ->
            case filterType of
                Equality ->
                    if key == name then
                        List.map stringContentOfNamedBlock blocks |> String.join ""

                    else
                        ""

                Contains ->
                    if String.contains key name then
                        List.map stringContentOfNamedBlock blocks |> String.join ""

                    else
                        ""

        _ ->
            ""


filterBlockByName : String -> Block -> String
filterBlockByName key block =
    case block of
        Block name blocks _ ->
            if key == name then
                List.map stringContentOfNamedBlock blocks |> String.join ""

            else
                ""

        _ ->
            ""


stringContentOfNamedBlock : Block -> String
stringContentOfNamedBlock block =
    case block of
        Paragraph exprMList _ ->
            listExprMToString exprMList

        VerbatimBlock _ strings _ _ ->
            String.join "\n" strings

        Block _ blocks _ ->
            List.map stringContentOfNamedBlock blocks |> String.join "\n"

        BError str ->
            str


stringValueOfList : List ExprM -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : ExprM -> String
stringValue text =
    case text of
        TextM str _ ->
            str

        ExprM _ textList _ ->
            String.join " " (List.map stringValue textList)

        ArgM textList _ ->
            String.join " " (List.map stringValue textList)

        VerbatimM _ str _ ->
            str

        ErrorM str ->
            str


textToString : ExprM -> String
textToString text =
    case text of
        TextM string _ ->
            string

        ExprM _ textList _ ->
            List.map textToString textList |> String.join "\n"

        ArgM textList _ ->
            List.map textToString textList |> String.join "\n"

        VerbatimM _ str _ ->
            str

        ErrorM str ->
            str
