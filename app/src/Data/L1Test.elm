module Data.L1Test exposing (text)


text =
    """

[title L1 Test]

[h2 Introduction]

L1 is a markup language with a Lisp-like syntax for the running text.  Running text consists of ordinary text and [i L-expressions],  Here is an L-expression: `[bold L-expression]`.  It is rendered like this: [bold L-expression].  Apart from the standard L-expressions, there are two non-standard ones:

|| code
   `a[i] = 0`               # for code
   $a^2 + b^2 + c^2  = 0$   # for math, TeX-style

L1 also has a notion of [i block]. Here is an example:

|| code
   | indented
      first line
      second

| indent
   Democritus (/dɪˈmɒkrɪtəs/; Greek: Δημόκριτος, Dēmókritos, meaning "chosen of the people";
   c.460 – c.370 BC) was an Ancient Greek pre-Socratic philosopher primarily remembered today
   for his formulation of an atomic theory of the universe. (([red Why does the second
   block not appear?])).

   Democritus was born in Abdera, Thrace,`![4]` around 460 BC, although there are disagreements
   about the exact year. His exact contributions are difficult to disentangle from those of
   his mentor Leucippus, as they are often mentioned together in texts. Their speculation
   on atoms, taken from Leucippus, bears a passing and partial resemblance to the
   19th-century understanding of atomic structure that has led some to regard
   Democritus as more of a scientist than other Greek philosophers; however,
   their ideas rested on very different bases.[`![5]` Largely ignored in ancient
    Athens, Democritus is said to have been disliked so much by Plato that
    the latter wished all of his books burned.`![6]` He was nevertheless well
    known to his fellow northern-born philosopher Aristotle, and was the
    teacher of Protagoras.`![7]`

[h2 Example sentences]

Compare the rendered text that you see here (right-hand side) with corresponding text on the left.

- L1 is a markup language with a syntax [i somewhat] like Lisp, but with square brackets instead of parentheses.

- Yes, a [b [red very]] bold move indeed! [i Note that macros are composing properly]: the source text is ` [b [red very]]`.

- Links are written as `[link New York Times https://nytimes.com]` and are rendered like this: [link New York Times https://nytimes.com].

- Inline math text is written as in this example, `$a^2 + b^2 = c^2$`, and is rendered as $a^2 + b^2 = c^2$.

- The simplest way to display an image is via this model: `[image URL]`, e.g. as below

[image https://images.pexels.com/photos/416179/pexels-photo-416179.jpeg?auto=compress&cs=tinysrgb&dpr=1&w=500]

[h2 Blocks]

A verbatim block begins with `||` at the left margin, followed by a space, and then followed by the name of the block.  Below is an example of a code block.  The lines in the body of the block must  be indented by three or more spaces.  This means that you can have "blank" lines in a verbatim block, so long as they have at least three leading spaces..  The line  between the fourth and the six line is iike this.

|| code
   || code
      first line
        second line
               third line
      fourth line

      sixth line

Another example is the [i math] block. Here is an example:


|| code
   || math
      \\int_0^1 x^n dx
         =
      \\frac{1}{n+1}

It is rendered like this:

|| math
   \\int_0^1 x^n dx
    =
   \\frac{1}{n+1}

[h2 Title, headings and table of contents]

Every L1 document has a title, as in this model: `[title Intro to Logic]`.  It also has an automatically  generated active table of contents which is gnerated from the section headings.  Active means that clicking on an item in the table of contents makes the document scroll to the corresponding heading.  Clicking on the heading makes the document scroll back to the table of contents.  Headings are of the form `[h2 abc]`, `[h3 abc]`, etc.  The document title is in essence an `h1` heading.


"""
