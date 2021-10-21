module Data.MiniLaTeXTest exposing (text)


text =
    """


1. Indentation error


\\begin{theorem}
Pythagoras sez: $a^2 + b^2 = c^2$.  Cool!!

2.  Missing tag



\\begin{theorem}
  Pythagoras sez: $a^2 + b^2 = c^2$.  Cool!!


3. OK


\\begin{theorem}
  Pythagoras sez: $a^2 + b^2 = c^2$.  Cool!!
\\end{theorem}


"""


text3 =
    """

\\title{Notes on Quantum Field Theory}


\\xlink{uuid:6f5a573d-5603-4470-8dc9-b0972997a6e6}{Notes on Quantum Field Theory}

\\begin{mathmacro}
   \\newcommand{\\bra}[0]{\\langle}
   \\newcommand{\\ket}[0]{\\rangle}
   \\newcommand{\\caF}[0]{\\mathcal{F}}
   \\newcommand{\\boR}[0]{\\bf{R}}
\\end{mathmacro}

\\setcounter{section}{9}

\\section{Fourier transform}


The \\term{Fourier transform} is defined by

\\begin{equation}
   (\\caF g )(k) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^\\infty g(x) e^{-ikx} dx
\\end{equation}

The Fourier transform is a unitary operator on $L^2(\\boR )$ with inverse

\\begin{equation}
   (\\caF^{-1} g)(x) =  \\frac{1}{\\sqrt{2\\pi}} \\int_{\\infty}^\\infty g(k) e^{-ikx} dk
\\end{equation}

One often writes $\\hat g$ for $\\caF(g)$. 

The fact that $\\caF$ is invertible allows us to write

\\begin{equation}
   g(x) = (\\caF^{-1} \\caF g)(x) =  \\frac{1}{\\sqrt{2\\pi}}  \\int_{-\\infty}^\\infty \\hat g(k)  e^{ikx} dk
\\end{equation}

The fact that the Fourier transform is unitary is \\term{Plancherel's theorem}.  Thus $g(x)$ 
isa superposition of  functions $e^{ikx}$ which appear with weight $\\hat g(k)$.  In Dirac's notation, we proceed formally to write this equation as

\\begin{equation}
   g(x)  = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^\\infty \\hat g(k) | k \\ket dk
\\end{equation}

where $| k \\ket = e^{ikx}$. Now $\\hat g(k) = (1/\\sqrt{2\\pi} )\\bra k | g \\ket$, so the integral can be written as

\\begin{equation}
   g(x)  = \\frac{1}{2\\pi} \\int_{-\\infty}^\\infty  | k \\ket \\bra k | g \\ket dk
\\end{equation}

Still proceeding formally, we write his as

\\begin{equation}
   {\\bf 1} = \\frac{1}{2\\pi} \\int_{-\\infty}^\\infty dk | k \\ket \\bra k |  
\\end{equation}

This is the resolution of the identity in the case of continuous spectrum.




\\subsection{Examples}

Let $f(x) = e^{-\\lambda x}$ for $x > 0$, $f(x) = 0$ for $x < 0$.  This is a sudden but exponentially decaying pulse. Then

\\begin{align}
   \\hat f(k) &= \\frac{1}{\\sqrt{2\\pi}}\\int_0^\\infty e^{-\\lambda x} e^{ikx} dx \\
   &= \\frac{1}{\\sqrt{2\\pi}} \\frac{-1}{ik + \\lambda}\\Big\\vert_0^\\infty \\
    &= \\frac{1}{\\sqrt{2\\pi}(ik + \\lambda)}
\\end{align}

In the same manner, we find that if $f(x) = e^{\\lambda x}$ for $x < 0$, $f(x) = 0$ for $x > 0$, then

\\begin{equation}
   \\hat f(k) =  \\frac{1}{\\sqrt{2\\pi}(ik - \\lambda)}
\\end{equation}


These are functions which decay exponentially at infinity.  Consider next a rectangular pulse $r_a(x)$, where $r_a(x) = 1/a$ for $x \\in [-a/2,a/2]$,  and where $r_a(x) = 0$ in the complement of the interval $[-a/2,a/2]$.  The height of the pulse is chosen so that the area under the graph is 1.  With this choice

\\begin{equation}
   \\int_{-\\infty}^\\infty r_a(x-\\xi)f(x) dx = \\text{av}_{a, \\xi}(g)
\\end{equation}

where the average is the average of the function on an interval of width $a$ with center $\\xi$.  The Fourier transform is given by 

\\begin{align}
   \\frac{1}{a}\\int_{-a/2}^{a/2} e^{-ikx} dx
   &= \\frac{1}{-ika} e^{-ikx} \\Big\\vert_{x=-a/2}^{x=a/2} \\
   &= (2/ka)\\sin ka/2 \\
   &= \\text{sinc}(ka/2)
\\end{align}

Thus the Fourier transform of the rectangular pulse is the sinc function, up to a scale factor:

\\begin{equation}
   \\hat r_a(k) = \\frac{1}{\\sqrt{2\\pi}}\\text{sinc}(ka/2).
\\end{equation}

Consider now the limit

\\begin{equation}
   \\lim_{a \\to 0}\\int_{-\\infty}^\\infty r_a(x-\\xi) f(x) dx = \\lim_{a\\to 0} av_{a,\\xi} f = f(\\xi)
\\end{equation}

We ask: can we pass the limit under the integral sign, and if so, what is that limit? The answer is yes, provided that we view convergence in the sense of convergence for linear functionals, the functional being

\\begin{equation}
   f \\mapsto \\int_{-\\infty}^\\infty r_a(x-\\xi) f(x) dx 
\\end{equation}

Given that caveat, we write

\\begin{equation}
   \\lim_{a \\to 0} r_a(x-\\xi)  = \\delta(x - \\xi)
\\end{equation}

This is the famous Dirac delta function, which we view mathematically as a distribution and physically as the idealization of a unit area spike concentrated near $\\xi$.
It is characterized by its action on functions:

\\begin{equation}
   \\label{diracdelta1}
    \\int_{-\\infty}^\\infty \\delta(x-\\xi)f(x)dx = f(\\xi)
\\end{equation}



We ask next:  what is the Fourier transform of $\\delta$?
A tentative answer is that it is the limit of the Fourier transforms of the rectangular pulses $r_a(x)$.  Therefore let us think about the limit of the functions $\\text{sinc}(ak/2)$ as $a$ tends to zero.  The first node to the right of the origin occurs at $k = 2\\pi/a$.  Thus the width of the principal lobe of the sinc function, which has height 1, increases without bound as $a$ tends to zero.  In other words, 

\\begin{equation}
   \\lim_{a\\to 0} \\text{sinc}(ka/2)  = 1
\\end{equation}

We conclude that 

\\begin{equation}
   \\hat \\delta = \\frac{1}{\\sqrt{2\\pi}}
\\end{equation}

where equality is equality of distributions.

\\subsection{ODE's and Green's functions}

The Fourier transform satisfies a plethora of beautiful and useful identities.  We discuss just a few of these here, then give an application to solving ODE's with constant coefficients.  First, the Fourier transform of a derivative:

\\begin{align}
   \\caF(f')(k) &= \\frac{1}{\\sqrt{2\\pi}}\\int_{\\infty}^{\\infty} f'(x) e^{-ikx}dx \\
   &= -ik\\frac{1}{\\sqrt{2\\pi}}\\int_{\\infty}^{\\infty} f'(x) e^{-ikx}dx
\\end{align}

We integrated by parts, assuming that the function $f(x)$ and its derivative decay at infinity at least as fast as $(1 + |x|)^{1/2 + \\epsilon}$.  Thus differentiation of a function corresponds to multiplication of its Fourier transform by $-ik$:

\\begin{equation}
   \\caF(f')(k) = -ik\\caF(f)
\\end{equation}

More generally, consider any polynomial $P(s)$.  The $P(d/dx)$ is a constant-coefficient differential operator.  We have

\\begin{equation}
   \\caF(P(d/dx)f)(k) = P(-ik)\\caF(f)
\\end{equation}

The fact the Fourier transform converts differentiation into multiplication means that differential equations can be solved by a combination of ordinary algebra and Fourier analysis.  Consider, for example, the first order equation 

\\begin{equation}
   \\label{ode1}
    u' - \\lambda u = f
\\end{equation}

Its Fourier transform is

\\begin{equation}
   -ik \\hat u - \\lambda \\hat u = \\hat f
\\end{equation}

Solving for $\\hat u$, we have

\\begin{equation}
   \\hat u = \\frac{-\\hat f}{ik + \\lambda}
\\end{equation}

Applying the inverse Fourier transform, we have

\\begin{align}
   u(x) &= \\caF^{-1}\\frac{-\\hat f}{ik + \\lambda} \\
   &= \\frac{-1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{\\infty} \\frac{ \\hat f(k) }{ik + \\lambda}e^{ikx} dk \\
   &= \\frac{-1}{2\\pi} \\int_{-\\infty}^{\\infty} \\left[  \\int_{-\\infty}^{\\infty} f(x')e^{-ikx} dx' \\right] 
   \\frac{e^{ikx}}{ik + \\lambda}dk \\
   &=  \\frac{-1}{2\\pi} \\int_{-\\infty}^{\\infty} \\left[  \\int_{-\\infty}^{\\infty} \\frac{e^{ik(x-x')}} {ik + \\lambda} dk \\right] f(x')dx'
\\end{align}

Thus we have

\\begin{equation}
   \\label{convolution1}
   u(x) = \\int_{-\\infty}^{\\infty} G(x-x') f(x') dx'
\\end{equation}

where 

\\begin{equation}
   \\label{green1}
   G(x-x')  = \\frac{-1}{2\\pi} \\int_{-\\infty}^{\\infty} \\frac{e^{ik(x-x')}} {ik + \\lambda}dk
\\end{equation}

From this solution to a simple problem, many lessons can be learned.  First, notice the form of  \\eqref{convolution1}/  It is the \\term{convolution} of two functions $G(x)$ and $f(x)$.

The general definition is

\\begin{equation}
   f*g(x) = \\int_{-\\infty}^{\\infty} f(x-y)g(y)dy
\\end{equation}

The function $G(x)$ in \\eqref{green1} is called the \\term{Green's function}.  Thus the solution to equation \\eqref{ode1} is given by convolution with the Green's function:

\\begin{equation}
   u = G*f
\\end{equation}

There is more to say about the Green's function.  First, note what happens when we differentiate a convolution:

\\begin{align}
   (f*g)'(x) &= \\frac{d}{dx} \\int_{-\\infty}^{\\infty} f(x-y)g(y)dy \\
   &= \\int_{-\\infty}^{\\infty} \\frac{d}{dx}  f(x-y)g(y)dy \\
\\end{align}

so that 

\\begin{equation}
   (f*g)'(x)= f'*g(x)
\\end{equation}

This identity holds more generally for any differential operator with constant coefficients:

\\begin{equation}
   L(f*g)(x)= (Lf)*g(x)
\\end{equation}

Returning to our equation $Lu = f$, where $Lu  = u' -\\lambda u$, we have $u = G*f$ as general solution.  Substitute back into the ODE to obtain $L(G*f) = f$.  
Apply the above identity to write this as $LG*f = f$.
Here $f$ is abitrary (within reason) and so $LG$ reveals itself as the identity element for the operation of convolution.  The question is: is there such an object?
There is  a hint in equation  \\eqref{diracdelta1}, which looks almost like a convolution.  Now the delta function is (among other things) the limit of a sequence of even functions, and therefore is itself even: $\\delta(-x) = \\delta(x)$.  Thus we may write \\eqref{diracdelta1}as

\\begin{equation}
   \\label{diracdelta2}
    \\int_{-\\infty}^\\infty \\delta(\\xi-x)f(x)dx =  f(\\xi)
\\end{equation}

In other words,

\\begin{equation}
    \\delta*f = f.
\\end{equation}

If $(LG)* f = f$ for all $f$, then $LG = \\delta$.  We conclude that the Green's function for $Lu = f$ is a solution of $LG = \\delta$.  This solution (which is not be unique if $L$ has a null space), is called the \\term{fundamental solution}.  From it, all other solutions are deduced by convolution.


\\subsection{More about convolution}

The operation of convolution satisfies many pleasant and useful properties.  One is that convolution of $g$ with $f$ tends to smooth out $g$ and increase its support.  
To illustrate this, let $f = r_a$ be  rectangular pulse of unit area supported on $[-a/2, a/2]$ considered above.  For any even function $f$, we have 

\\begin{align}
   (f*g)(x) &= \\int_{-\\infty}^\\infty  f(x - y) g(y) dy \\
    &= \\int_{-\\infty}^\\infty  f(y -x) g(y) dy
\\end{align}

Let $(T_a f)(y) = f(x-a)$ be the translation operator.  Thus the graph of $T_af$ is the graph of $f$ shifted $a$ units to the right, and the integral above can be written as

\\begin{equation}
   (f*g)(x) = \\int_{-\\infty}^\\infty T_x(f)(y)g(y)  dy
\\end{equation}

Therefore

\\begin{equation}
   (r_af*g)(x) =\\frac{1}{a} \\int_{x - a/2 }^{x + a/2 } f(y)g(y)  dy = \\overline{g_a}(x),
\\end{equation}

where $ \\overline{g_a}(x) $ is the average of $g(x)$ on $[x - a/2, x + a/2]$.  Averaging a function smooths it out, addressing the first stated property.  It also increases support.  If $g$ is supported on the interval $[b,c]$, then $r_a*g$ is supported on the larger interval $b - a, c + a]$.  In general, the large the support of $f$, where $f(x) \\ge 0$ for all $x$, the large is the support of $f*g$.  Indeed, if $f$ is supported on $[b,c]$ and the width of the support of $f$ is $d$, then $f*g$ is supported on $[b - d, c + d]$ -- an interval larger by $2d$ units

\\image{http://psurl.s3.amazonaws.com/images/jc/convolution2-4598.png}{Convolution}{align: center, width: 400}

\\subsection{References}

\\href{http://ocw.mit.edu/courses/physics/8-05-quantum-physics-ii-fall-2013/lecture-notes/MIT8_05F13_Chap_04.pdf}{Dirac's Bra and Ket notation} -- Notes from B. Zwiebach's course at MIT

\\href{http://www.physics.iitm.ac.in/~labs/dynamical/pedagogy/vb/delta.pdf}{All about the Dirac delta function} -- V. Balakrishnan, IIT Madras

\\href{http://math.arizona.edu/~kglasner/math456/fouriertransform.pdf}{Fourier transform techniques} -- U. Arizona notes

\\href{https://www.math.utah.edu/~gustafso/s2013/3150/pdeNotes/fourierTransorm-PeterOlver2013.pdf}{Fourier transform} -- 

\\href{http://www.physics.rutgers.edu/~steves/501/Lectures_Final/Lec06_Propagator.pdf}{Olver notes, Free particle propagator}

\\href{http://www.reed.edu/physics/faculty/wheeler/documents/Miscellaneous%20Math/Delta%20Functions/Simplified%20Dirac%20Delta.pdf}{Delta function} -- Reed college notes






"""
