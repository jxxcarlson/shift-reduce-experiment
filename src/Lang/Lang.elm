module Lang.Lang exposing (Lang(..), equationBlockNames, theoremLikeNames)


type Lang
    = L1
    | MiniLaTeX
    | Markdown


theoremLikeNames =
    [ "theorem", "proposition", "corollary", "lemma", "definition", "problem", "question" ]


equationBlockNames =
    [ "equation", "align" ]
