%%% releasenotes.cls

\ProvidesClass{releasenotes}

\def\ProvidesClass#1[#2]{\relax}

% \input article.cls
\LoadClass{article}

\def\ps@plain{\let\@mkboth\@gobbletwo
     \let\@oddhead\@empty\def\@oddfoot{\reset@font
     \footer}\let\@evenhead\@empty\let\@evenfoot\@oddfoot}
\def\footer{\hfil \parbox[t]{6in}{\hfil Chez Scheme Version {\thisversion} Release Notes Page \thepage\\
  \strut\hfil \copyright~{\pubyear} Cisco Systems, Inc.  Licensed under the Apache License Version 2.0\\
  \strut\hfil http://www.apache.org/licenses/LICENSE-2.0}\hfil}

\newif\ifthisversion\thisversionfalse
\newif\ifthatversion\thatversionfalse
\newif\ifpubmonth\pubmonthfalse
\newif\ifpubyear\pubyearfalse
\def\thisversion#1{\thisversiontrue\def\thisversion{#1}}
\def\thatversion#1{\thatversiontrue\def\thatversion{#1}}
\def\pubmonth#1{\pubmonthtrue\def\pubmonth{#1}}
\def\pubyear#1{\pubyeartrue\def\pubyear{#1}}

\def\maketitle{%
  \ifthisversion\else\@latex@warning{`thisversion' undefined}\fi
  \ifthatversion\else\@latex@warning{`thatversion' undefined}\fi
  \ifpubmonth\else\@latex@warning{`pubmonth' undefined}\fi
  \ifpubyear\else\@latex@warning{`pubyear' undefined}\fi
  \begingroup\Large\bfseries
  \begin{flushleft}
  Chez Scheme {\thisversion} Release Notes\\
  {\pubmonth}~{\pubyear}
  \end{flushleft}
  \endgroup}

%%% to support hypertext index entries
\def\hindex#1{\index} % ignore the label here---no links in printed version

\def\hyperlink#1#2{#2}
\newcommand{\href}[3][ref]{#3}

\newif\iflatex\latextrue
\newif\ifhtml\htmlfalse

%%% latex.ltx redefinitions
%% leave padding page blank (no header)
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
    {\pagestyle{empty}\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi}\fi\fi}
%% change section headers to "1.1. foo" instead of "1.1  foo"
\def\@seccntformat#1{\csname the#1\endcsname. }
%% change numbered table of contents lines to "1.1. foo" instead of "1.1  foo"
\def\numberline#1{#1. }

\renewenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\renewcommand*\descriptionlabel[1]{\hspace\labelsep #1}

\def\itemvdots{\item[] \mbox{\vdots}}

\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

%%% grammar support
\newenvironment{grammar}
  {\begingroup
    \def\orbar{&\bar&}
    \def\nobar{&&}
    \def\longis{&$\longrightarrow$&}
   \penalty-100\vskip 4pt\begin{tabular}{lcl}}
  {\end{tabular}\penalty-200\vskip 4pt\endgroup}
\def\bar{$\vert$}
\def\ang#1{$\langle${\small\rm{}#1}$\rangle$}
\def\kstar{\raise.5ex\hbox{\scheme{*}}}
\def\kplus{\raise.5ex\hbox{\scheme{+}}}

\newcounter{alphacount}
\def\alphalabel{\textit{\alph{alphacount}}.}
\newenvironment{alphalist}
  {\begingroup\let\beforeschemedisplay=\relax\let\afterschemedisplay=\relax
   \begin{list}{\alphalabel}{\usecounter{alphacount}\itemsep=0pt\parsep=0pt%
        \topsep=0pt}}
  {\end{list}\endgroup}

%%% final set up
\renewcommand{\contentsname}{Contents}
\renewcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{plain}
\pagenumbering{arabic}

\if@twoside\else\raggedbottom\fi

\usepackage{fullpage}
\usepackage{scheme}

\schemeindent=0pt
\parskip=4pt
\parindent=0pt

\input macros.tex

\endinput
%%% end of assignment.cls
