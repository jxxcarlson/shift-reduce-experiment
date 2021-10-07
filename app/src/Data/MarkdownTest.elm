module Data.MarkdownTest exposing (text)


text =
    """

# Markdown test

It was a  *very* bold proposal.

- This is some text. This is some math. This is some math.This is some math.This is some math. This is some math. his is some math. This is some math. This is some math. This is some math.

- This is some text. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math. This is some math.



## Math

Pythagoras said that $a^2 + b^2 = c^2$.  In calculus class, we learned that

$$
   \\int_0^1 x^n dx = \\frac{1}{n+1}


## Code

This is some code:  `a := 1`.  And so is this

```
   a[1] = 1
   b[1] = 1

   c[i] = 2
   d[i] = c[i] + 1


## Quotation

This is a quote:

>
   Regular languages are rather inexpressive, 
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



*by James Carlson*
"""
