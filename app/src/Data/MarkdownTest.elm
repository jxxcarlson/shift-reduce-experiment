module Data.MarkdownTest exposing (text)


text =
    """
[!title](Krakow Talk: Lambda Days)



# Introduction

The  principal aim of this talk to is to show how one can build a fault-tolerant parser for a class of markup languages using a functional language.  Any statically typed language should do, but here we use [Elm](https://elm-lang.org) .  It is sufficiently expressive for the job and has the advantage of being designed for web apps, which is what we have in mind: a platform for creating and distributing technical content with both images and mathematical notation.

## Multiple Languages

Our current implementation handles three languages:

- L1, with a Lisp-like syntax, e.g., `[bold stuff]` for

- Rational Markdown, like Markdown, but with some differences and some extensions

- MiniLaTeX, a subset-variant of LaTeX.

While the three languages are quite different from one another,  they are all block-structured, and indentation is significant, as in Python.  The integrated editor make management of indentation easy,  Here are some examples.

### L1

A code block:

```
   || code
      sum = 0
      for k in range(1,101):
         sum = 1.0/k

### Rational Markdown



### MiniLaTeX


there is a single parser and a single AST.  With a codebase of around 4000 lines, the language-specific parts are small: around 750 lines of code total with three files for each language.

The single AST means that one needs just one function to render HTML.  There is also have a function converts the AST in to standard LaTeX.   All documents can be exported to LaTeX.  As a corollary, we offer a conversion to PDF service for documents in all three languages.


## Fault Tolerance

By _fault-tolerant,_ we mean that when the parser encounters a syntax error, it corrects the AST it is building so that current error is noted  in a an unobtrusive, helpful way and the following text is not disturbed.  In this I have been very much inspired by the error messages of the Elm compiler. Here is an example:


EXAMPLE


Note that the text below the error is not messed up, as is often the case.  Seeing the mesage, you realize that you forgot to put the closing `$$` tag.  If you insert it, you get this:

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$


# Implementing Fault Tolerance

Our fault-tolerant parser consists of the following stages

. Break the text into chunks

"""


text2 =
    """
*Itemized list*

- Milkshakes

- Sausages

*Numbered list*

. Put gas in the car

. Drive to Las Vegas
"""


text1 =
    """
[! title](Markdown Test)

_The L3 project provides a real-time, fault-tolerant compiler to HTML for three markup languages: L1, with a Lisp-like syntax, Markdown (a flavor thereof), and MiniLaTeX, a subset-variant of LaTeX._

# Introduction

This is a test document for the L3 implementation of Markdown.  It is pretty much the same as classical Markdown, but there are some differences.  One feature/difference up front — you can render math in the usual LaTeX way, like this:


$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}
$$

To see how this was done, look at the source text.  (Click on *Show Editor* to view the source.)  If you forget the trailing `$$` in a math block, you get this:

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}

Notice that even though there is an error above this line,  what follows it is not messed up.  That is because L3 is built with a fault-tolerant parser.  It tries to recover from errors as best it can, signalling their presence in a helpful but not obtrusive way in the rendered text.

Inline math text is also done in the usual way, like this `$a^2 + b^2 = c^2$:`

> Pythagoras said that $a^2 + b^2 = c^2$. This was a long time ago, but nonetheless he was right. Quite the dude!

Another difference: the body of a any block is assumed to be indented with respet to the the block header (`$$` in the example above).  Indentation should be three spaces.  Of course the indentation requirement is a huge pain without a proper editor built into the app.  We don't have that feature yet, but it s coming.

Notice, by the way, that a table of contents is generated automatically.  The title is the heading beginning with a single hash mark `#`, and there should only be one of these.   The table of contents will provide active links to sections of the document in a soon-to-be released upgrade.


# Code

This is some code:  `a := 1`.  And so is this

```
   a[1] = 1
   b[i] = 2


Notice the "hanging" style for this block. It works because of the indentation requirement: a block ends when the indentation level goes down.  Like Python.


# Quotation

This is a quote:

>  Regular languages are rather inexpressive,
   but they work great for lexers. On the opposite
   side of expressivity spectrum are Turing machines.
   For them, we also have a number of meta-languages
   (like Rust), which work great for humans.
    It’s interesting that a Turing machine is
    equivalent to a finite state machine with
    a pair of stacks: to get two stacks from a tape,
    cut the tape in half where the head is. Moving
    the head then corresponds to popping from one
    stack and pushing to another.

# Images

All software projects should have a mascot.  Here is ours:


![Bird](https://images.pexels.com/photos/416179/pexels-photo-416179.jpeg?auto=compress&cs=tinysrgb&dpr=1&w=500)

Images are sized automatically to fill the page from left to right.  We will have some Markdown extension with options for image size and placement, and also captions.


# Plans

We'll get to the below as soon as we can.  However, we are completely preocuppied at the moment by the task of finishing version 1 of the L3 compiler — bug fixes, implementing the remaining parts.

- Implement export to  LaTeX and to PDF.  (Those buttons are there, but they don't do anything yet.  Sorry, soon!)

- SVG rendering

- Substitute the L3 compiler for the corresponding code at MiniLaTeX.io.


We welcome and appreciate suggestions, but can't promise to adopt them.



"""
