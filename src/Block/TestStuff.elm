module Block.TestStuff exposing (l1, m1, t1, t2, t3, t4, t5, t6)


l1 =
    "| foo\n   a\n   b\n   | bar\n      c\n      d"


m1 =
    "\\begin{code}\n   abc\n   def\n\\end{code}\n\nyada\nnada\n\n\\begin{math}\n   xyz\n\\end{math}"


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
