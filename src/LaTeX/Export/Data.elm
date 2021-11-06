module LaTeX.Export.Data exposing (preamble)

{-| A standard preamble
-}


preamble : String -> String -> String -> String
preamble title author date =
    """
\\documentclass[11pt, oneside]{article}

%% Packages
\\usepackage{geometry}
\\geometry{letterpaper}
\\usepackage{changepage}   % for the adjustwidth environment
\\usepackage{graphicx}
\\usepackage{wrapfig}
\\graphicspath{ {images/} }
\\usepackage{amssymb}
\\usepackage{amsmath}
\\usepackage{amscd}
\\usepackage{hyperref}
\\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,
    urlcolor=blue,
}
\\usepackage{xcolor}
\\usepackage{soul}


%% Commands
\\newcommand{\\code}[1]{{\\tt #1}}
\\newcommand{\\ellie}[1]{\\href{#1}{Link to Ellie}}
% \\newcommand{\\image}[3]{\\includegraphics[width=3cm]{#1}}

\\newcommand{\\imagecenter}[1]{
   \\medskip
   \\begin{figure}
   \\centering
    \\includegraphics[width=12cm,height=12cm,keepaspectratio]{#1}
    \\vglue0pt
    \\end{figure}
    \\medskip
}

\\newcommand{\\imagefloatright}[3]{
    \\begin{wrapfigure}{R}{0.30\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\imagefloatleft}[3]{
    \\begin{wrapfigure}{L}{0.3-\\textwidth}
    \\includegraphics[width=0.30\\textwidth]{#1}
    \\caption{#2}
    \\end{wrapfigure}
}

\\newcommand{\\italic}[1]{{\\sl #1}}
\\newcommand{\\strong}[1]{{\\bf #1}}
\\newcommand{\\subheading}[1]{{\\bf #1}\\par}
\\newcommand{\\xlink}[2]{\\href{{https://minilatex.lamdera.app/g/#1}}{#2}}
\\newcommand{\\red}[1]{\\textcolor{red}{#1}}
\\newcommand{\\blue}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\violet}[1]{\\textcolor{violet}{#1}}
\\newcommand{\\remote}[1]{\\textcolor{red}{#1}}
\\newcommand{\\local}[1]{\\textcolor{blue}{#1}}
\\newcommand{\\highlight}[1]{\\hl{#1}}
\\newcommand{\\note}[2]{\\textcolor{blue}{#1}{\\hl{#1}}}
\\newcommand{\\strike}[1]{\\st{#1}}
\\newcommand{\\term}[1]{{\\sl #1}}
\\newtheorem{remark}{Remark}
\\newcommand{\\comment}[1]{}
\\newcommand{\\innertableofcontents}{}

%% Theorems
\\newtheorem{theorem}{Theorem}
\\newtheorem{axiom}{Axiom}
\\newtheorem{lemma}{Lemma}
\\newtheorem{proposition}{Proposition}
\\newtheorem{corollary}{Corollary}
\\newtheorem{definition}{Definition}
\\newtheorem{example}{Example}
\\newtheorem{exercise}{Exercise}
\\newtheorem{problem}{Problem}
\\newtheorem{exercises}{Exercises}
\\newcommand{\\bs}[1]{$\\backslash$#1}
\\newcommand{\\texarg}[1]{\\{#1\\}}

%% Environments
\\renewenvironment{quotation}
  {\\begin{adjustwidth}{2cm}{} \\footnotesize}
  {\\end{adjustwidth}}

\\def\\changemargin#1#2{\\list{}{\\rightmargin#2\\leftmargin#1}\\item[]}
\\let\\endchangemargin=\\endlist

\\renewenvironment{indent}
  {\\begin{adjustwidth}{0.75cm}{}}
  {\\end{adjustwidth}}


\\definecolor{mypink1}{rgb}{0.858, 0.188, 0.478}
\\definecolor{mypink2}{RGB}{219, 48, 122}

\\newcommand{\\fontRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\textcolor{mycolor}{#4}
    }

\\newcommand{\\highlightRGB}[4]{
    \\definecolor{mycolor}{RGB}{#1, #2, #3}
    \\sethlcolor{mycolor}
    \\hl{#4}
     \\sethlcolor{yellow}
    }

\\newcommand{\\gray}[2]{
\\definecolor{mygray}{gray}{#1}
\\textcolor{mygray}{#2}
}

\\newcommand{\\white}[1]{\\gray{1}[#1]}
\\newcommand{\\medgray}[1]{\\gray{0.5}[#1]}
\\newcommand{\\black}[1]{\\gray{0}[#1]}

% Spacing
\\parindent0pt
\\parskip5pt


\\begin{document}


\\title{""" ++ title ++ """}
\\author{""" ++ author ++ """}
\\date{""" ++ date ++ """}

\\maketitle

\\tableofcontents

"""
