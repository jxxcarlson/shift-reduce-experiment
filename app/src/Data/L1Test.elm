module Data.L1Test exposing (text)


text =
    """
[title L1 Test]

L1 is a markup language with a syntax [i somewhat] like Lisp,
but with square brackets instead of parentheses.
It is similar to and linguistically a descendant of
CaYaTeX, work of this author and Nicholas Yang,
but with more robust error-handling abilities.

Yes, a [b [red very]] bold move indeed! [i (Note that macros are composing properly].


[h2 Puzzle!]

[link NYT https://nytimes.com]


[link https://nytimes.com `New York Times` ]

[h1 blocks]

[h2 code]

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
