module Data.MiniLaTeXTest exposing (text)


text =
    """

% logic-macros


\\begin{mathmacro}
  \\newcommand{\\opensets}{\\mathcal{O}}
  \\newcommand{\\opensetsB}{\\mathcal{O}}
  \\newcommand{\\set}[1]{\\{ #1 \\}}
  \\newcommand{\\for}{\\mathcal{F}}
  \\newcommand{\\axioms}{\\mathcal{A}}
  \\newcommand{\\theorems}{\\mathcal{T}}
  \\newcommand{\\lang}{\\mathcal{L}}
  \\newcommand{\\reals}{\\mathbb{R}}
\\end{mathmacro}


\\setcounter{section}{4}

\\title{Intuitionistic logic} \\tags{mtest}



This section, at least for now, is an exegis of some of the problems in \\cite{RH}, chapter 3 and of the material in \\cite{WKIL}.  We begin with a formal system J for intuitionistic logic. It has one rule of inference, Modus Ponens, and ten axioms \\cite{RH}, p. 109:

\\strong{J1} $A \\to (B \\to A)$.

\\strong{J2} $[A \\to (B \\to C)] \\to [(A \\to B) \\to (A \\to C) $

\\strong{J3} $(A \\land B) \\to A$

\\strong{J4} $(A \\land B) \\to B$

\\strong{J5} $A \\to (B \\to (A \\land B)) $

\\strong{J6} $A \\to (A \\lor B)$

\\strong{J7} $A \\to (B \\lor A$

\\strong{J8} $(A \\to C) \\to [(B \\to C) \\to ((A \\lor B) \\to C)] $

\\strong{J9} $(A \\to B) \\to [(A \\to \\neg B) \\to \\neg A ] $

\\strong{J10} $\\neg A \\to (A \\to B) $

From these, one can derive various theorems using the same procedure used for system P.


\\subsection{Soundness}

To speak about the soundness of intuitionistic logic, one must first have
a notion of interpretation, that is, a notion of homomorphism $\\phi: \\for \\to \\mathcal{A}$ for a suitable structure $\\mathcal{A}$. In classical logic, the right structure is the 2-element Boolean algebra  with  elements variously denoted $T$ and $F$, or $1$ and $0$, or $\\top$ and $\\bot$.  That a Boolean algebra is the right structure follows from the fact that (1) the basic operations of propositional logic are $\\land$, $\\lor$ and $\\neg$, (2) the operations denoted by the same symbols are the basic operations of a Boolean algebra, and (3) it makes sense to demand that
$\\phi$ is compatible with these structures, that is: $\\phi(a \\lor b) = \\phi(a) \\lor \\phi(b)$, $\\phi(a \\land b) = \\phi(a) \\land \\phi(b)$,
and $\\phi(\\neg a) = \\neg \\phi(a)$.






\\subsection{The Law of the Excluded Middle}

The main result of this section is that classical  and intuitionistic logic are different, e.g., they have different theorems. In particular:

\\begin{theorem}  \\label{lem} The Law of the Excluded Middle is not a theorem of system J.
\\end{theorem}

To show that there is no proof of the Law of the Excluded Middle in system J, we consider interpretations $\\phi : \\for \\to \\opensets(\\reals)$,  the algebra of open subsets of the real line.
A basic open set is an open interval $(a,b) = \\set{ x : a < x < b }$ and a general open set $U$ is an arbitrary union of basic open sets. An open set  $U$ in $\\reals$ has the property that for any point $x \\in U$, there is an open  interval $(x -\\epsilon, x + \\epsilon)$ that contains $x$ and is contained in $U$.

Union and intersecton define operations on $\\opensets(\\reals)$, with the  $\\empty$ and the set of all real numbers $\\reals$ playing the role of the neutral element  union and interection.
For the operation of complement, more care is required.  The set-theoretic complement of an open set is closed but generally not open: consider the interval $(\\infty, 0)$. Its complement is the ray $[0, \\infty) = \\set{ x : 0 \\le x < \\infty}$.  It is not open, because there is no small open interval containing the end-point $0$ that is contained in $[0, \\infty)$.  There is, however, a way out: define the \\term{pseudocomplement} of $U$, denoted $\\neg U$ to the the largest open subset of the set-theoretic complement.  For example, if $U$ is the ray $(-\\infty, 0)$ then the largest open subset of its set-theoretic complement $[0, \\infty)$ is the open ray $(0, \\infty)$.


For the proof, let $\\phi$ be an interpretation in which $\\phi(A) = (a, \\infty)$ for some formula $A$ and some real number $a$.  Thus we have

$$
 \\phi(A \\lor \\neg A) = \\phi(A) \\cup \\phi(\\neg A) = \\phi(A) \\cup \\neg \\phi(A)
$$

This reads more simply as

$$
 \\phi(A \\lor \\neg A) = (a,\\infty) \\cup (-\\infty, a) = \\set{ x : x \\neq 0 } \\neq \\reals
$$

Suppose that the Soundness Theorem holds for system J (we will prove it shortly). Then every provable formula is true.  The above computation shows that the Law of the Excluded Middle is not true.  Therefore it is not provabe.




\\subsection{STUFF}

Now take $U$ to the set $\\set{ x \\in \\reals :  x > 0}$.  Then $\\neg U = \\set{ x \\in \\reals :  x < 0}$.    Consequently $U \\cup \\neg U = \\set{x \\in \\reals : x \\ne 0} \\ne \\top$.  Because the Heyting algebra $\\opensets(\\reals)$ is set up to express the failure of the Law of the Excluded Middle, just as $Bool = \\set{\\bot, \\top}$ is set up to express its validity, it is not a surprise that such algebras can be used to formulate and prove a Soundness Theorem.  Note the element of topology that comes into play: negation is not set-theoretic complementation, but rather topological pseudo-complementation.




Turning to the proof of Theorem \\ref{lem},  we fix an integer $n > 0$ and define an interpretatiion $\\phi: \\for \\to \\set{0, \\ldots, n}$ which satisfies the laws


\\begin{enumerate}

  \\item $\\phi(a \\lor b) = \\min \\phi(A), \\phi(B)$

  \\item  $\\phi(a \\land b) = \\max \\phi(A), \\phi(B)$

  \\item If $\\phi(A) \\ge \\phi(B)$, then $\\phi(A \\to B) = 0$.  Otherwise,
  $\\phi(A \\to B) = \\phi(B)$.

  \\item If $\\phi(A) = n$, then $\\phi(\\neg A) = 0$.  Otherwise, $\\phi(\\neg A) = n$.

\\end{enumerate}

Note that if $n = 1$, then $\\phi(A \\lor \\neg A) = 0$.  Indeed, if $\\phi(A) = 0$, then $\\phi(\\neg A) =1$, and so $\\phi(A \\lor \\neg A) = \\min 0, 1 = 0$.  With $\\phi(A) = 1$, one reaches the same conclusion.  We sometimes say that \\italic{$A$ has level $k$ if $\\phi(A) = k$.}

\\begin{lemma}
 (1) Let $A$ be an axiom of system J. Then $\\phi(A) = 0$. (2) Suppose that $\\phi(A) = 0$ and that $\\phi(A \\to B) = 0$.  Then $\\phi(B) = 0$.
\\end{lemma}

\\strong{Proof.}  Routine verification.

\\begin{theorem}
  Let $A$ be a theorem of system J.  Then $\\phi(A) = 0$.
\\end{theorem}

\\strong{Proof.}  Repeated application of the Lemma. \\strong{Q.E.D.}



\\strong{Proof of Theorem \\ref{lem} } . For the proof, consider the interpretation defined above for the case $n =3$.  If $\\phi(A) = 0$, then $\\phi(\\neg A) = 2$, and so $\\phi(A \\lor \\neg A) = 0$.  However, if $\\phi(A) = 1$, then $\\phi(\\neg A) = 2$, and in this case  $\\phi(A \\lor \\neg A) = 1$.  A theorem must satisfy $\\phi(A) = 0$ in all interpretations, so this is a contradiction. \\strong{Q.E.D.}

For an example of a theorem in classical logic that is not a theorem in intuitionistic logic, consider the formula $D_n$ which is the disjunction of the formale $p_i \\leftrightarrow p_j$ for all $i, j$ satisfying $1 \\le i < j \\le n$. In classical logic, the formula $D_2$ is not valid, but $D_n$ for $n \\ge 3$ is valid.  Check this for $n = 3$ using truth tables. That will give the idea for the combinatorics of the general case. Next, observe that  if $\\phi(A) \\ne \\phi(B)$,  then

$$
  \\phi(A \\leftrightarrow B) = \\max \\phi(A), \\phi(B)
$$

and

$$
  \\phi(A \\leftrightarrow B) = 0
$$

in the contrary case.  Using this fact, one may assign values to the $p_i$ to show that for $n  \\ge 2$, there is an interpretation with values in $\\mathbf{n}$ such that  $\\phi(D_n) \\ne 0$.  For example, with $n = 3$, we may take $\\phi(p_i) = i-1$.
Then

\\begin{align}
 \\phi(D_3) &= \\phi((p_1 \\leftrightarrow p_2) \\lor (p_1 \\leftrightarrow p_3)\\lor (p_2 \\leftrightarrow p_2)) \\\\
  & = \\min \\phi(p_1 \\leftrightarrow p_2), \\phi(p_1 \\leftrightarrow p_3), \\phi(p_3 \\leftrightarrow p_3) \\\\
  & = \\min 1, 2, 2 \\\\
 & = 1 \\\\
\\end{align}


In general, one finds that $\\phi(D_n) > 0$, so that  $D_n$ is not a theorem of J.
The fact that increasingly larger models are needed to express the semantics of system J suggests. but does not prove that no finite model suffices.

((If axiom J10 is replaced by $\\neg \\neg A \\to A$, tthe law of the excluded middle, then the resulting system yields classical propositional logic.))


\\subsection{Applying Heyting Semantics}

We will now give an applciation of the Heyting algebra $\\opensets(\\reals)$ considered above \\mdash  the algebra of open subsets of the real line.  To this end, let us give the formal definition. A Heyting algebra conists of a partially ordered set $(H, \\le)$ endowed with operations $\\land$ and $\\lor$ which make it into a distributive lattice.  In addition, there are greatest and least elements, $\\top$ and $\\bot$, respectively.  Finally, there is a operation $\\to$ which satisfies $z \\le (x \\to y)$ if and only if $z \\land x \\le y$.  Every Boolean algebra is a Heyting algebra, where $x \\to y = \\neg x \\lor y$.  A Heyting algebra which is complemented in the sense that $x \\lor \\neg x = \\top$ is a Boolean algebra.

The only piece lacking in our description of  $\\opensets(\\reals)$ as a Heyting algebra
is the operation $U \\to V$,  It is defined as as

$$
 U \\to V = \\bigcup \\set{W\\in \\mathcal{O} : W \\cap U \\subset V }
$$

It is easy to see that $W \\cap U \\subset V$ if and only if $W \\subset (U \\to V)$.  For computations, we will use the formulation of the Lemma below.  If we omit taking thei interior  (or use the discrete topology) in this definition and the defintion of negation, we recover classical logic.

\\begin{lemma}
 $U \\to V = Int(U^c \\cup V)$.
\\end{lemma}

Recall our computation of $U \\cup \\neg U = \\set{ x \\ne 0 } \\ne \\reals$ in the case $U = \\set{ x > 0}$.  In view of this fact, $\\opensets(\\reals)$ is not a Boolean algebra.
The elementary properties bellow tell us that we can compute in $\\opensets(\\reals)$ as we do in the Boolean algebra of subsets, \\strong{except} that negation is a topological operation: one uses pseudocomplement instead of complement.

\\begin{enumerate}

\\item  If $A \\subset B$, then $A \\to B = \\reals$.

\\item If $A \\cap B = \\emptyset$, then $A \\to B = \\neg A$.

\\item $\\bot \\to B = \\top$ and $A \\to \\bot = \\neg A$.

\\end{enumerate}

The above facts follow from the definiiton, or directly from the definition.  Assertion (1) is clear, since $U \\cap A \\subset U \\cap B \\subset B$ for all $U$.  For (2), let $U$ be an open set such that $U \\cap A \\subset B$.  Since $U \\cap A \\subset A$, one has $U \\cap A = \\emptyset$, and therefore $A \\to B \\subset \\neg A$.
Now suppose $U \\cap A = \\emptyset$.  Then $U \\cap A \\subset B$.  Therefore the reverse implication holds.  Assertion (3) is a special case of (2).

\\subheading{Computations}

Let $\\phi$ be an interpretation of J in $\\opensets(\\reals)$ given by a function $\\phi$ on the propositional variables to $\\opensets(\\reals)$ which extends to the formulae according to the
rules below.

\\begin{enumerate}

\\item $\\phi(\\bot) = \\emptyset$

\\item $\\phi(\\top) = \\reals$

\\item $\\phi(A \\lor B) = \\phi(A) \\cup \\phi(B)$

\\item $\\phi(A \\land B) = \\phi(A) \\cap \\phi(B)$

\\item $\\phi(\\neg A) = \\neg \\phi(A)$

\\item $\\phi(A \\to B) = \\phi(A) \\to \\phi(B)$.

\\end{enumerate}

\\begin{lemma} The axioms of system J are satisfied by any $\\opensets(\\reals)$-valued
 interpretation.
\\end{lemma}

We  prove the Lemma in the case of axiom J1, that is, we show that

$$
 \\phi(A \\to (B \\to A) = \\reals
$$

for any $\\opensets(\\reals)$-valued interpretation.  To this end, let $U = \\phi(A)$ and let $V = \\phi(B)$.  These are open subsets of the real line.  Then we have the following sequence of reductions:


\\begin{tabular}{lll}
  1 & $U \\to (V \\to U)$ & HYP \\\\
  2 & $= Int (U^c \\cup (V \\to U))$ & Def \\\\
  3 & $= Int (U^c \\cup (Int(V^c \\cup U)))$ & Def \\\\
  4 & $ \\supset Int(U^c \\cup Int(V^c) \\cup Int(U))$ & topology \\\\
  5 & $\\supset Int(U^c \\cup Int(U))$ & clear \\\\
  5 & $\\supset Int(U^c \\cup U)$ & open set \\\\
  6 & $\\supset Int(\\reals)$ &clear \\\\
  6 & $\\reals$ & open set \\\\
\\end{tabular}


The remark labeled "topology" is the statement $Int(X) \\cup Int(Y) \\subset Int(X \\cup Y)$.

The next lemma says that Modus Ponens operates as it should
on the level of semantics.

\\begin{lemma}
 If $\\phi(A) = \\reals$ and $\\phi(A \\to B) = \\reals$, then $\\phi(B) = \\reals$.
\\end{lemma}

\\strong{Proof.} Let $U = \\phi(A)$ and $V = \\phi(B)$ be the open sets corresponding to the given propositions.  Then $U = \\reals$ and $Int( U^c \\cup V) = \\reals$.  From the first assertion, it follows that $U^c = \\emptyset$.  Then the second assertion reduces to $Int(V) = \\reals$, which implies $V = \\reals$. \\strong{Q.E.D.}

\\begin{theorem}
 Suppose that $\\vdash_J A$, and suppose that $\\phi$ is an $\\opensets(\\reals)$-valued interpretation.  Then $\\models_\\phi A$.
\\end{theorem}

\\subheading{Notes for further work}

\\cite{RH} sketches an argument based on the formulas $D_n$ to show that no set of functions $\\phi: \\for \\to B$ for $B$ a finite set can express the semantics of system J. I've used \\cite{RH}, \\cite{WKIL} and \\cite{EPS} as sources for the above discussion.




%% the semantics for intuitionisitc logic.  Semantics in this kind of logic has a very different character than the semantics of classical logic, where one usually takes interpretations with values in the $Bool$, the 2-element Boolean algebra.  The interpretations we will use in the argument below take values in the set  $\\mathbf{n} = \\set{0, \\ldots, n}$ for various values of $n$.


%%Interpretations with values in a finite set are quite useful, but are not sufficiently powerful to prove the Soundness Theorem. To prove this fundamental result, we shall use \\term{Heyting algebras}.  An example of such an algebra is the algebra $\\opensets(\\reals)$ of open subsets of the real line in the usual topology.  We think of $\\opensets(\\reals)$ as a sophisticated "topological version" of the power set $2^X$. It is endowed with the operations of union and intersection and with partial order $U \\subset V$.  With these operations, it is a complete distributive lattice. Here "complete" means that there are least and greatest elements with respect to the partial order, namely $\\emptyset$ and $\\reals$.  We write these $\\bot$ and $\\top$. For a given open set $U$, one defines the operation $\\neg U$ to be the largest open set contained in the complement of $U$:


"""
