module Data.L1Test exposing (text)


text =
    """

[title L1 Test]

[h2 Intro]

Some features:

- L1 is a markup language with a syntax [i somewhat] like Lisp, but with square brackets instead of parentheses.

- Yes, a [b [red very]] bold move indeed! [i (Note that macros are composing properly)].

- Links are written as `[link New York Times https://nytimes.com]` and are rendered like this: [link New York Times https://nytimes.com].

- The simplest way to display an image is via this model: `[image URL]`, e.g. as below

[image https://images.pexels.com/photos/416179/pexels-photo-416179.jpeg?auto=compress&cs=tinysrgb&dpr=1&w=500]

[h2 Blocks]

[h2 Code]

|| code
   \\int_0^1 x^n dx
      =
   \\frac{1}{n+1}

It is rendered as

|| math
   \\int_0^1 x^n dx
    =
   \\frac{1}{n+1}



"""
