module Block.TestStuff exposing (l1, m1, t1, t2, t3, t4, t5, t6)


l1 =
    "| foo\n   a\n   b\n   | bar\n      c\n      d"


m1 =
    """
$$
   \\int_0^1

## Code
"""


t1 =
    """
one two three
four five six
"""


t2 =
    """
> Regular languages are rather inexpressive,
   but they work great for lexers. On the opposite 
   side of expressivity spectrum are Turing machines. 
"""


t3 =
    """
one two three
four five six
    
> Regular languages are rather inexpressive,
   but they work great for lexers. On the opposite 
   side of expressivity spectrum are Turing machines. 
"""


t4 =
    """1

2
"""


t5 =
    """
1
2
    
>  3
   4
   5 
"""


t6 =
    """abc def
ghi jkl

one two three
four five six
"""
