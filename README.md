# A Versatile Markup-to-Html Compiler


```
"\\section{Birds}" =>
[SParagraphS ["\\section{Birds}"] BlockComplete] =>
[ParagraphS [ExprS "heading2" [TextS "Birds"],TextS "\n"] BlockComplete]

"## Birds" =>
[SBlockS "heading2" [SParagraphS ["Birds"] BlockStarted] BlockStarted] =>
[BlockS "heading2" [ParagraphS [TextS "Birds\n"] BlockStarted] BlockStarted]
 
 So we need the following
 
 [SBlockS "heading2" [SParagraphS ["Birds"] BlockStarted] BlockStarted] =>
 [ParagraphS [ExprS "heading2" [TextS "Birds"],TextS "\n"] BlockComplete]

in the Block parser
```

This document describes compiler that transforms markup languages to HTML.  It has the following characteristics:

- The parser is fast enough to operate in real time in an interactive editing environment.  By this, we mean that documents of reasonable size can be re-parsed and re-rendered on each keystroke without noticeable lag to the user.

- The parser is fault-tolerant.  If the user makes a syntax error, that fact is noted immediately in the rendered text. The text at fault is highlighted and the nature of the error is briefly described.

- The parser can handle a variety of markup languages.  In the current setup, these languages are L1, whose syntax is inspired by Lisp, Rational Markdown, similar to Markdown, and MiniLaTeX, similar to LaTeX. The dependency of the parser on a given Language is via two small modules, one which defines a function for reducing a stack of tokens to an AST value, the other which classifies the initial lines of blocks of text into types, e.g., ordinary paragraph, block, etc.  Thus to implement a new new language one must write new code for these modules.  Just how broad the class of languages that can be handled this way is an open question that we have not considered.

- A common AST type is used for all markup languages considered. Thus a single function, independent of language, suffices to render documents regardless of language.  The AST can also be rendered to standard LaTeX.  This provides a way to render documentsto PDF.

Text, which is given as a list of lines, is processed as follows.  First, it  parsed to  `StringBlocks`, where 

```
type StringBlock
    = SParagraph (List String) Meta
    | SVerbatimBlock String (List String) Meta
    | StringBlock String (List StringBlock) Meta
    | SError String
```
  
Blocks can be things such as ordinary paragraphs, quotations in Markdown, environments in LaTeX, etc.  The metadata component carries information such as the line number of the first and last line in the source text.  A second parser is then mapped over `List SBlock` to produce a `List Block`, where

```
type Block
    = Paragraph (List Expr) Meta
    | VerbatimBlock String (List String) ExpressionMeta Meta
    | Block String (List Block) Meta
    | BError String
```

Value of type `Expr` capture ordinary text as well as in-line text elements that are marked in some way, e.g., bold text in markdown or LaTeX (`*bold text*` or `\strong{bold text}`), mathematical text delimited by dollar signs, e.g., `$ a^2 + b^2 = c^2 $`, etc.
 

```
type Expr
    = ExprText String ExpressionMeta
    | ExprVerbatim String String ExpressionMeta
    | Expr String (List Expr) ExpressionMeta
```

The `ExpressionMeta` component holds more detailed information the location of the substring of the source text corresponding to the given element, as well as a unique id for the element.  The id resurfaces as an id of the  element rendered as HTML.  This makes source mapping easy: clicking the rendered text runs a function that retrieves the position information the AST.  One can use this information (for example), to highlight the corresponding source text.

## Parsing Inline Text

A small parser built with parser combinators chomps tokens from the input string.  These are consumed, one  at a time, by a shift-reduce parser.  This parser maintains a data structure, its `State`, which has various fields, the most important of which are:
	
- input: String
- committed: List Expr
- stack: List (Either Token Expr)


Thus elements of the stack can represent either tokens or `Expr` values.

The shift-reduce loop is straightforward.  Each time a token is acquired from the source, it is pushed onto the stack.  The a reduce function is applied. This function operates by matching patterns against an initial segment of the stack.  When a match is found the matched tokens are used to (a) construct a simpler stack item or (b) an expression.  In case (a) the new item replaces the matched tokens on the stack.  In case (b), the expression is prepended to the stack.  If no match is found, the state is left unchanged.


If the shift-reduce parser consumes the entire input string and finds itself with a non-empty stack, it is in an error state.  The parser then attempts to recover from the error by modifying the stack, inserting both a correction and a notice of error, and the trying to reduce it once again.  (EXPLAIN WHY NO INFINITE LOOP)

## Parsing Blocks

The block parser is similar in nature except that tokens are lines ... (TO BE CONTINUED)  