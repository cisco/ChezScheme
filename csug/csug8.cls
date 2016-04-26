%%% csug8.cls
%%% Based on tspl4.cls
%%% Copyright (c) 1998 R, Kent Dybvig
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
%%% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{csug8}[2009/07/12 CSUG8]

\newif\ifdropfolios\dropfoliosfalse

\newif\if@openright
\newif\if@mainmatter \@mainmattertrue
\newif\ifchapterpictures \chapterpicturestrue
\newif\ifdraft\draftfalse
\newcommand{\@ptsize}{}

%%% to support hypertext index entries
\def\hindex#1{\index} % ignore the label here---no links in printed version

\newcommand{\hyperlink}[3][ref]{#3}
\newcommand{\href}[3][ref]{#3}
\newcommand{\hpageref}[3][ref]{#3}
\let\true@ref=\ref\renewcommand{\ref}[2][]{\true@ref{#2}}
\let\true@pageref=\pageref\renewcommand{\pageref}[2][]{\true@pageref{#2}}

\newif\iflatex\latextrue
\newif\ifhtml\htmlfalse

\newlength{\trimwidth}
\newlength{\trimheight}
\newlength{\gutterwidth}
\newlength{\edgewidth}

% paperheight is total height of paper before trimming
% paperwidth is total width of paper before trimming
% trimwidth is amount that will be trimmed on outside (unbound) edge
% trimheight is amount that will be trimmed on both top and bottom
% gutterwidth is margin on the inside (bound) edge
% edgewidth is margin on all the outside (unbound) edges

% text height will be paperheight - 2*trimheight - 2*edgeheight
% text width will be paperwidth - trimwidth - gutterwidth - edgewidth
% inside (bound) margin will be gutterwidth
% outside (unbound) margins will be edgewidth

%%% options
\DeclareOption{crownquarto}
% lulu lies or their converter is broken and we shouldn't include the trim
%   {\setlength\paperheight {25.235cm}%  % 24.6cm + 2 * .125
%    \setlength\paperwidth  {19.535cm}%  % 18.9cm + .25in
%    \setlength\gutterwidth{1.0in}%
%    \setlength\edgewidth{1.0in}%
%    \setlength\trimwidth{.25in}%
%    \setlength\trimheight{.125in}}
   {\setlength\paperheight {24.6cm}%
    \setlength\paperwidth  {18.9cm}%
    \setlength\gutterwidth{1.0in}%
    \setlength\edgewidth{1.0in}%
    \setlength\trimwidth{0in}%
    \setlength\trimheight{0in}}
\DeclareOption{tspl4size}
   {\setlength\paperheight {9.25in}%
    \setlength\paperwidth  {7.0in}%
    \setlength\gutterwidth{.875in}%    % isn't this actually 1in?
    \setlength\edgewidth{1.0in}%
    \setlength\trimwidth{.125in}%
    \setlength\trimheight{.125in}}
\DeclareOption{ninebysix}
   {\setlength\paperheight {9.25in}%
    \setlength\paperwidth  {6.125in}%
    \setlength\gutterwidth{.75in}%
    \setlength\edgewidth{.75in}%
    \setlength\trimwidth{.125in}%
    \setlength\trimheight{.125in}}
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\DeclareOption{10pt}{\renewcommand{\@ptsize}{0}}
\DeclareOption{11pt}{\renewcommand{\@ptsize}{1}}
\DeclareOption{12pt}{\renewcommand{\@ptsize}{2}}
\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\drafttrue}
\DeclareOption{final}{\draftfalse}
\DeclareOption{titlepage}
    {\ClassError{proc}{Option `titlepage' is not supported}{}}
\DeclareOption{notitlepage}{\relax}
\DeclareOption{openright}{\@openrighttrue}
\DeclareOption{openany}{\@openrightfalse}
\DeclareOption{onecolumn}{\relax}
\DeclareOption{twocolumn}
    {\ClassError{proc}{Option `twocolumn' is not supported}{}}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\ExecuteOptions{crownquarto,10pt,twoside,onecolumn,final,openright}
\ProcessOptions
\input{csug81\@ptsize.clo}
 
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand{\baselinestretch}{}
% block paragraphs:
\setlength\parskip{4\p@ \@plus \p@}
\setlength\parindent{0\p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand{\topfraction}{.7}
\setcounter{bottomnumber}{1}
\renewcommand{\bottomfraction}{.3}
\setcounter{totalnumber}{3}
\renewcommand{\textfraction}{.2}
\renewcommand{\floatpagefraction}{.5}
\setcounter{dbltopnumber}{2}
\renewcommand{\dbltopfraction}{.7}
\renewcommand{\dblfloatpagefraction}{.5}

%%% headers and footers
\if@twoside
  \def\ps@headings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \def\chaptermark##1{%
      \markboth{\if@mainmatter\thechapter.\ \fi##1}%
               {\if@mainmatter\thechapter.\ \fi##1}}
    \def\sectionmark##1{%
      \markright{\thesection.\ ##1}}}
\else
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \def\chaptermark##1{%
      \markright{\if@mainmatter\@chapapp\ \thechapter.\fi\ ##1}}}
\fi
\newcommand*{\chaptermark}[1]{}
\setcounter{secnumdepth}{2} % must be at least two
\newcounter {chapter}
\newcounter {section}[chapter]
\newcounter{exercise}[section]
\renewcommand{\thechapter}{\arabic{chapter}}
\renewcommand{\thesection}{\thechapter.\arabic{section}}
\renewcommand{\theexercise}{\thechapter.\arabic{section}.\arabic{exercise}}
\newcommand{\@chapapp}{\chaptername}

%%% illustrated chapter heads
\newlength{\chframesize}
\setlength{\chframesize}{\textwidth}
\addtolength{\chframesize}{-\fboxrule}
\addtolength{\chframesize}{-\fboxrule}
\newlength{\chpicsize}
\setlength{\chpicsize}{\chframesize}
\addtolength{\chpicsize}{-6pt}
\def\chpic#1{\begingroup%
  \fboxsep=3pt
  \fbox{\includegraphics[height=\chpicsize]{#1}}\endgroup}
%\def\chpic#1{\begingroup%
%  \fboxsep=0pt
%  \vbox{\noindent%
%    \fbox{\vbox{\hbox to \chframesize{\hfil\vbox to \chframesize{\vfil%
%      \includegraphics{#1}\vfil}\hfil}}}}\endgroup}

%\def\picturechapterhead#1{
%  \thispagestyle{empty}
%  \null\vfill\vfill
%  {\LARGE\bfseries\hbox to \textwidth{\hfil CHAPTER \thechapter}}
%  \vfill}

\def\picturechapterhead#1{
  \thispagestyle{empty}
  \vbox to 6pc{\null\vfill
  {\Large\hbox to \textwidth{\hfil CHAPTER \thechapter}}
  \hbox to \textwidth{\leaders\hrule\hfil}}
  \vskip 10pt
  {\titlefont\hbox to \textwidth{\hfil#1}}
  \vfill\vfill\vfill\noindent
  \chpic{pic/ch\thechapter}
  \par\eject
  \thispagestyle{empty}
  \null
  \vfill
  \noindent
  {\it \input{pic/ch\thechapter.tex}}\par\break
  \thispagestyle{empty}}

%%% document structure
\newcommand{\frontmatter}{\cleardoublepage
            \@mainmatterfalse\pagenumbering{roman}}
\newcommand{\mainmatter}{\cleardoublepage
       \@mainmattertrue\pagenumbering{arabic}}
\newcommand{\backmatter}{\if@openright\cleardoublepage\else\clearpage\fi
      \@mainmatterfalse}
\def\chapter#1{
  \if@openright\cleardoublepage\else\clearpage\fi
  \global\@topnum\z@
  \if@mainmatter
    \refstepcounter{chapter}%
    \typeout{\@chapapp\space\thechapter.}%
    \addcontentsline{toc}{chapter}%
              {\protect\numberline{\thechapter}#1}%
  \else
    \addcontentsline{toc}{chapter}{#1}%
  \fi
  \addtocontents{lof}{\protect\addvspace{10\p@}}%
  \addtocontents{lot}{\protect\addvspace{10\p@}}%
  \chaptermark{#1}%
  \if@mainmatter
    \ifchapterpictures
      \picturechapterhead{#1}%
    \else
      \plainchapterhead{#1}%
    \fi
  \else
    \plainchapterhead{#1}%
  \fi
  \@afterindentfalse
  \@afterheading}
\def\plainchapterhead#1{%
  \ifdropfolios\thispagestyle{plain}\else\thispagestyle{empty}\fi%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \reset@font
    \interlinepenalty\@M
    \if@mainmatter
      \titlefont\makebox[\hsize][l]{\thechapter. #1}\par\nobreak
    \else
      \titlefont\makebox[\hsize][l]{#1}\par\nobreak
    \fi
    \vskip 40\p@
  }}
\newcommand{\section}{\@startsection{section}{1}{\z@}%
                                   {-3.5ex \@plus -1ex \@minus -.2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\reset@font\Large\bfseries}}

%%% page layout
\setlength\leftmargini  {2.5em}
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\setlength\leftmarginv  {1em}
\setlength\leftmarginvi {1em}
\setlength\leftmargin    {\leftmargini}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand{\theenumi}{\arabic{enumi}}
\renewcommand{\theenumii}{\alph{enumii}}
\renewcommand{\theenumiii}{\roman{enumiii}}
\renewcommand{\theenumiv}{\Alph{enumiv}}
\newcommand{\labelenumi}{\theenumi.}
\newcommand{\labelenumii}{(\theenumii)}
\newcommand{\labelenumiii}{\theenumiii.}
\newcommand{\labelenumiv}{\theenumiv.}
\renewcommand{\p@enumii}{\theenumi}
\renewcommand{\p@enumiii}{\theenumi(\theenumii)}
\renewcommand{\p@enumiv}{\p@enumiii\theenumiii}
\newcommand{\labelitemi}{$\m@th\bullet$}
\newcommand{\labelitemii}{\normalfont\bfseries --}
\newcommand{\labelitemiii}{$\m@th\ast$}
\newcommand{\labelitemiv}{$\m@th\cdot$}

\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{3\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*{\cal}{\@fontswitch{\relax}{\mathcal}}
\DeclareRobustCommand*{\mit}{\@fontswitch{\relax}{\mathnormal}}

%%% table of contents
\newcommand{\@pnumwidth}{1.55em}
\newcommand{\@tocrmarg} {2.55em}
\newcommand{\@dotsep}{4.5}
\setcounter{tocdepth}{2}
\newcommand*{\l@chapter}[2]{%
  \addpenalty{-\@highpenalty}%
  \vskip 3pt \@plus4\p@
  \setlength\@tempdima{1.5em}%
  \begingroup
    \parindent \z@ \rightskip \@pnumwidth
    \parfillskip -\@pnumwidth
    \leavevmode \bfseries
    \advance\leftskip\@tempdima
    \hskip -\leftskip
    #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
    \penalty\@highpenalty
  \endgroup}
% \newcommand*{\l@section}      {\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand*{\l@section}[2]{%
  \vskip \z@ \@plus2\p@
  {\leftskip 1.5em\relax \rightskip \@tocrmarg \parfillskip -\rightskip
   \parindent 1.5em\relax\@afterindenttrue
   \interlinepenalty\@M
   \leavevmode
   \@tempdima 2.3em\relax
   \advance\leftskip \@tempdima \null\nobreak\hskip -\leftskip
   {#1}\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par}}
\newcommand{\tableofcontents}{%
  \if@openright\cleardoublepage\else\clearpage\fi
  \ifdropfolios\thispagestyle{plain}\else\thispagestyle{empty}\fi%
  \global\@topnum\z@
  \chaptermark{\contentsname}%
  \plainchapterhead{\contentsname}%
  \@starttoc{toc}}

%%% bibliography
\newdimen\bibindent \bibindent=1.5em
\newcommand{\newblock}{}
\newenvironment{thebibliography}[1]
     {\chapter{References}
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \renewcommand{\newblock}{\hskip .11em \@plus.33em \@minus.07em}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%%% index
\newenvironment{theindex}
               {\if@openright\cleardoublepage\else\clearpage\fi
                \begingroup\raggedright\schemeindexsize\footnotesize
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\plainchapterhead{\indexname}]%
                \addcontentsline{toc}{chapter}{\indexname}%
                \chaptermark{\indexname}%
                \ifdropfolios\thispagestyle{plain}\else\thispagestyle{empty}\fi\parindent\z@
                \indexintrotext\medskip
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\clearpage\endgroup}
\newcommand{\@idxitem}  {\par\hangindent 40\p@}
\newcommand{\subitem}   {\par\hangindent 40\p@ \hspace*{20\p@}}
\newcommand{\subsubitem}{\par\hangindent 40\p@ \hspace*{30\p@}}
\newcommand{\indexspace}{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}

%%% latex.ltx redefinitions
%% leave padding page blank (no header)
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
    {\pagestyle{empty}\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi}\fi\fi}
%% change section headers to "1.1. foo" instead of "1.1  foo"
\def\@seccntformat#1{\csname the#1\endcsname. }
%% change numbered table of contents lines to "1.1. foo" instead of "1.1  foo"
\def\numberline#1{#1. }

\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep #1}

\def\itemvdots{\item[] \mbox{\vdots}}

\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

\newenvironment{grammar}
  {\begingroup
    \def\orbar{\hbox to 2em{\hfil}$\vert$\hbox to .75em{\hfil}}
    \def\longis{ $\longrightarrow$\hbox to .75em{\hfil}}
   \penalty-100\vskip 6pt plus 1pt\parindent=0pt\interlinepenalty=5000}
  {\penalty-200\vskip6pt plus 1pt\endgroup}

\def\bar{$\vert$}
\def\ang#1{$\langle${\small\rm{}#1}$\rangle$}
\def\kstar{\raise.5ex\hbox{\scheme{*}}}
\def\kplus{\raise.5ex\hbox{\scheme{+}}}

% for fft example in examples.stex
\def\W#1{W_{\!\!#1}}
\def\fftcases#1{\left\{\,\vcenter{\m@th\baselineskip=18pt
  \ialign{$##\hfil$&\quad##\hfil\crcr#1\crcr}}\right.}

%%% adapted from old tspl macros.tex
%%% argument #1 is the \label{anslab} inserted by tspl4-prep
\def\exercise#1{
 \vskip 9pt plus 1pt minus 1pt\refstepcounter{exercise}\noindent
 {\bf Exercise \arabic{chapter}.\arabic{section}.\arabic{exercise}.#1~}}

\newcounter{alphacount}
\def\alphalabel{\textit{\alph{alphacount}}.}
\newenvironment{alphalist}
  {\begingroup\let\beforeschemedisplay=\relax\let\afterschemedisplay=\relax
   \begin{list}{\alphalabel}{\usecounter{alphacount}\itemsep=0pt\parsep=0pt%
        \topsep=0pt}}
  {\end{list}\endgroup}

\newdimen\formdefwidth\formdefwidth=\textwidth\advance\formdefwidth by -2.5pt
\def\entryheader{\par\penalty-200\vskip15pt plus 6pt\noskipentryheader}
\def\noskipentryheader{\vbox\bgroup\parskip=0pt
    \def\formdef##1##2{\par\begingroup\fboxsep=0pt\@@line{\colorbox{formdefcolor}{\hbox to \textwidth{\strut##2\hfil{\small\bf{##1}}}}\hss}\endgroup}
    \def\returns{\par\noindent{\small\bf returns:} }
    \def\libraries{\par\noindent{\small\bf libraries:} }}
\def\endnoskipentryheader\par{\egroup\nobreak\vskip6pt plus 1pt\relax}
\let\endentryheader=\endnoskipentryheader
\def\categorysyntax{syntax}
\def\categoryprocedure{procedure}
\def\categorythreadparameter{thread parameter}
\def\categoryglobalparameter{global parameter}
\def\categorymodule{module}
\def\categoryftype{ftype}

\newwrite\forms
\openout\forms=\jobname.rfm
% \formsummary{sort key}{type}{form}{label}
\def\formsummary{\begingroup\@sanitize\addsummary}
\def\addsummary#1#2#3#4{\endgroup
  \edef\formhead{\write\forms}%
  \edef\formentry{{"#1" \string\sfentry{#3}{#2}{\string\pageref{#4}}}}%
  \expandafter\formhead\formentry}
\def\sfentry#1#2#3{\par
 \hbox to \hsize{%
  \hbox to 24pc{#1\ \hfil}%
  \hbox to 5pc{#2\ \hfil}%
  \hfil #3}}
\newenvironment{thesummary}
  {\begingroup\schemesummarysize\small\bigskip
   \sfentry{{\slshape Form}}{{\slshape Category}}{{\slshape Page}}
   \kern3pt\hrule\kern3pt}
  {\endgroup}

\newwrite\answers
\openout\answers=\jobname.ans
\def\answer{\begingroup\@sanitize\addanswer}
\long\def\addanswer#1#2{\endgroup
  \edef\anshead{\write\answers}%
  \edef\ansentry{{\string\ansentry{#1}{#2}}}%
  \expandafter\anshead\ansentry}
\def\theanswers{\begingroup
  \long\def\ansentry##1##2{\par\vskip 9pt plus 3pt minus 1pt\noindent\textbf{Exercise~\ref{##2}.~}(page~\pageref{##2})\par\nobreak\vspace{6pt}##1}
  \immediate\closeout\answers
  \input \jobname.ans
  \endgroup}

%%% final set up
\newcommand{\contentsname}{Contents}
\newcommand{\bibname}{References}
\newcommand{\indexname}{Index}
\newcommand{\chaptername}{Chapter}
\newcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{headings}
\pagenumbering{arabic}

\if@twoside\else\raggedbottom\fi
\endinput
