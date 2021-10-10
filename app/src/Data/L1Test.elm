module Data.L1Test exposing (text)


text =
    """
[title L1 Test]

L1 is a markup language with a syntax [i somewhat] like Lisp,
but with square brackets instead of parentheses.

Yes, a [b [red very]] bold move indeed! [i (Note that macros are composing properly)].

Links are written as `[link New York Times https://nytimes.com]` and are rendered like this:

[link New York Times https://nytimes.com]


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
