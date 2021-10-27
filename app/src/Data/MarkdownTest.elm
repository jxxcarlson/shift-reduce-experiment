module Data.MarkdownTest exposing (text)



text = """



[! title](Welcome to Zipdocs)


*Login.* Not needed.  Just choose your language, click on *New Document*, and start writing.  But if you would like to
set up an account, go for it. (There are some advantages: you don't have to keep track of document links).
"""



text_ =
    """
[! title ](Krakow Talk: Lambda Days)



# Introduction

The  principal aim of this talk to is to show how one can build a fault-tolerant parser for a class of markup languages using a functional language.  Any statically typed language should do, but here we use [Elm](https://elm-lang.org) .  It is sufficiently expressive for the job and has the advantage of being designed for web apps, which is what we have in mind: a platform for creating and distributing technical content with both images and mathematical notation.

## Multiple Languages

Our current implementation handles three languages:

- L1, with a Lisp-like syntax for most things, e.g., `[bold stuff]` for bold text or `[bold [italic stuff]]` for bold italic text.  There are abbreviated forms, e.g., `[b [i stuff]]`. Inline mathematics is written as in TeX, e.g., `$a^2 + b^2 = c^2$`.

- XMarkdown, like Markdown, but with some differences and some extensions, e.g blocs for SVG images.  For example, while bulleted list items begin with a dash, numbered list items begin with a period.  Mathematical text is the same as in TeX. _Note to self.  Reconsider special forms, e.g., maybe make them like `@fName[ ... args ..]; also, fix italic and bold!_. Then we need a quoting mechanism for characters like "@".

- MiniLaTeX, a subset-variant of LaTeX.

While the three languages are quite different from one another,  they are all block-structured, and indentation is significant, as in Python or Haskell.  Except for paragraphs, blocks have a header, and the body of the block is more indented than the header.  A block can be terminated by an empty line or more generally by a less indented line. _More on this + do some thinking.  Shall we use Haskell's indentation system?_


### L1

A code block:

```
   || code
      sum = 0
      for k in range(1,101):
         sum = 1.0/k
```

### XMarkdown

SVG images:

```
   @svg
      (the svg source text)
```


### MiniLaTeX





there is a single parser and a single AST.  With a codebase of around 4000 lines, the language-specific parts are small: around 750 lines of code total with three files for each language.

The single AST means that one needs just one function to render HTML.  There is also have a function converts the AST in to standard LaTeX.   All documents can be exported to LaTeX.  As a corollary, we offer a conversion to PDF service for documents in all three languages.


## Fault Tolerance

By _fault-tolerant,_ we mean that when the parser encounters a syntax error, it corrects the AST it is building so that current error is noted  in an unobtrusive, helpful way and the following text is not disturbed. Below is an extended example.

### Stage 1

First, the source text:


```
  $$
  \\int_0^1 x^n dx = \\frac{1}{n+1}
```

When the source text is rendered, the text in error is highlighted and note is made of the error:



$$
\\int_0^1 x^n dx = \\frac{1}{n+1}

The line following the `$$` is not indented.

### Stage 2

We fix the indentation, but the block is not closed, so the system still highlights what we have written:


$$
  \\int_0^1 x^n dx = \\frac{1}{n+1}

The closing `$$` is missing:

### Stage 3

We insert the closing `$$`:

```
  $$
     \\int_0^1 x^n dx = \\frac{1}{n+1}
  $$
```

The rendered text is as we expect:

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$


# Implementing Fault Tolerance

Our fault-tolerant parser consists of the following stages

. Break the text into chunks of type `SBlock.` Their contents are strings.  This is carried out by a state machine that processes lines on the basis of a "classifier."  For example, a line that starts with`\\begin{equation}` is classified as `BeginVerbatimBlock` in MiniLaTeX.

. Parse the contents of the SBlocks to obtain `Blocks.` Their content is made of values of type `Expression` and `List Expression.`

```
   +-------------+       +------------+         +------------+
   |             |       |            |         |            |
   | Source text | ----> |  SBlocks   | ---->   |   Blocks   |
   |             |       |            |         |            |
   +-------------+       +------------+         +------------+
```

An immediate benefit of this strategy is that errors in parsing expressions cannot propagate beyond the the walls of the box in which they are housed.

"""
