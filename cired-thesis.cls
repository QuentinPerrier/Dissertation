% -------------------------------------------------------------------
%  @LaTeX-class-file{
%     filename         = "cired-thesis.cls",
%     version           = "0.1",
%     date                = "07 07 2017",
%     codetable       = "ISO/ASCII",
%     keywords        = "LaTeX, CIRED thesis",
%     docstring         = "A thesis class for a CIRED dissertation."
% --------------------------------------------------------------------

%This file defines the necessary packages

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cired-thesis}[2017/07/07 v0.1 Cired Thesis Class]
\RequirePackage{lineno}
\LoadClass[12pt, twoside, openright, a4paper]{book}

% Required packages
%\usepackage[utf8]{inputenc} 
%\usepackage[T1]{fontenc}
\RequirePackage{csquotes} %added
\RequirePackage{babel}
\RequirePackage{graphicx}
\RequirePackage[hyphens]{url}
\RequirePackage[linktoc=all]{hyperref}
\RequirePackage{appendix}

% colors
\RequirePackage{xcolor}
\definecolor{SchoolColor}{RGB}{0,121,126}  

\hypersetup{
    colorlinks,
    citecolor=SchoolColor,
    filecolor=black,
    linkcolor=black,
    urlcolor=SchoolColor,
}


\RequirePackage[centertags]{amsmath}

\RequirePackage{array}
\newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{R}[1]{>{\raggedleft\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}


\RequirePackage{amssymb}
\RequirePackage{amsthm}
\newtheorem{corollary}{Corollary}
\newtheorem{proposition}{Proposition}
\newtheorem{lemma}{Lemma}
\newtheorem{definition}{Definition}

%\RequirePackage{}
\RequirePackage[backend=biber,natbib=true,bibencoding=latin1, citestyle=authoryear, style=authoryear, url = false]{biblatex} %A choisir plutôt que natbib
\DefineBibliographyExtras{french}{\restorecommand\mkbibnamefamily}% Pour éviter les capitalisation des auteurs, qui est par défaut avec babel french
%\RequirePackage[sectionbib,round]{natbib}
\RequirePackage[small, md, sc]{titlesec}
\RequirePackage[tight,nice]{units}
\RequirePackage{verbatim}
\RequirePackage{framed}


\RequirePackage{lscape}

\RequirePackage{longtable}
%\RequirePackage{tabu}
\RequirePackage{multirow}
%\RequirePackage{chngpage}



\RequirePackage{enumerate}
\RequirePackage{booktabs}


\RequirePackage[titles]{tocloft}
\setcounter{tocdepth}{1}
\renewcommand{\cftchapfont}{\normalsize \scshape}

%\renewcommand\bibname{References}
\renewcommand\listfigurename{Listing of figures}
%\raggedright

\RequirePackage{afterpage} 

%   list an entire bibliography entry inline. Useful for acknowledging when my paper was previously published
\RequirePackage{bibentry} 
%\nobibliography*        

\RequirePackage{lettrine} % big letter at start of chapter
\RequirePackage[width=5in, a4paper]{geometry}

\RequirePackage{fancyhdr} 
\pagestyle{plain} % options: empty , plain , fancy
\RequirePackage[palatino]{quotchap}
%\definecolor{chaptergrey}{rgb}{0,121,0} %to change the color of chapter number, not working well

\RequirePackage[official]{eurosym}
\RequirePackage{rotating}
\RequirePackage{subfig}

\RequirePackage{titling}
\RequirePackage{setspace} 
\RequirePackage{booktabs} % for much better looking tables
\RequirePackage[labelfont={sf,bf,small},textfont={sf,small},margin=0pt, figurewithin=chapter, tablewithin=chapter]{caption}
\onehalfspacing
%\raggedright

\parindent 12pt
\RequirePackage{mathspec}
\RequirePackage{import}

% use Arno Pro instead of standard LaTeX fonts
\setmathsfont(Digits,Latin,Greek)[Numbers={OldStyle,Proportional}]{Arno Pro}
%\setmathrm{Arno Pro}

%\RequirePackage{fontspec,xltxtra,xunicode}
%\defaultfontfeatures{Mapping=tex-text, Numbers=OldStyle}
%\setromanfont[Mapping=tex-text, 
%			  Numbers=OldStyle, 
%			  Ligatures={Common}, 
%			  SizeFeatures={%
%		          {Size={-10}, Font=* Caption},
%		          {Size={10-13.0}, Font=*},
%		          {Size={13.01-19.9}, Font=* Subhead, Ligatures=Common, Contextuals={Swash,Alternate}},
%		       {Size={19.91-},Font=* Display, Ligatures=Common, Contextuals={Swash,Alternate}}}]
%			  {Arno Pro}

\RequirePackage{fontspec,xltxtra,xunicode}
\setromanfont[Mapping=tex-text,
BoldFont=Arno Pro Bold,
ItalicFont=Arno Pro Italic,
BoldItalicFont=Arno Pro Bold Italic]{Arno Pro}

\RequirePackage{multicol}
\RequirePackage{vwcol}

			 
%Set fonts for Mac Specific (Comment out on Windows XP)
%\setsansfont[Scale=MatchLowercase,Mapping=tex-text]{Arno Pro} 
%\setmonofont[Scale=MatchLowercase]{Consolas}

\RequirePackage{nomencl}
\makenomenclature
\renewcommand{\nomname}{Acronyms}



% some definitions
\def\degreeyear#1{\gdef\@degreeyear{#1}}
\def\degreemonth#1{\gdef\@degreemonth{#1}}
\def\jurycomposition#1{\gdef\@jurycomposition{#1}}
\def\degree#1{\gdef\@degree{#1}}
\def\advisor#1{\gdef\@advisor{#1}}
\def\department#1{\gdef\@department{#1}}
\def\field#1{\gdef\@field{#1}}
\def\university#1{\gdef\@university{#1}}
\def\universitycity#1{\gdef\@universitycity{#1}}
\def\universitystate#1{\gdef\@universitystate{#1}}

\def\frenchtitle#1{\gdef\@frenchtitle{#1}}


\renewcommand{\maketitle}{ 
	\singlespacing
	\thispagestyle{empty}
	\begin{center}
	\includegraphics[height=3cm]{frontmatter/logo_ehess.png}\\
	%\hspace{15em}
	\vspace*{\fill} 
	%\vspace{30pt} 
	\foreignlanguage{french}{
	\Huge \sc \@frenchtitle \\
	}
	\vspace{30pt}
	\Large \sc \thetitle \\ 
	\vspace{30pt}
	\singlespacing
	\large
	\foreignlanguage{french}{Thèse présentée par\\ \theauthor\\ 
	\vspace{8pt}
	pour obtenir le grade de \@degree\\
	de \@university \\
	\vspace{4pt}
	Discipline : \@field\\
	\vspace{4pt}
	%Thèse dirigée par \@advisor\\
	}
    \normalsize
	%\vspace{8pt}
	\end{center} \vspace*{\fill}
	%Soutenue le \@degreemonth\ \@degreeyear\ devant un jury composé de :\\
	Soutenue devant un jury composé de :\\
	\textnormal{
	\@jurycomposition\
	}
}

% You might also consider licensing your work under Creative Commons). See: http://creativecommons.org/weblog/entry/12824 for other PhD students who have released their work under creative commons.

\newcommand{\copyrightpage}{
	\newpage 
	\thispagestyle{empty} 
	\begin{center} 
 	\vspace*{\fill}	
	\sc \noindent \copyright~\textit{\@degreeyear \hspace{3pt}~- \theauthor} \\
	\noindent Some Rights Reserved. \\
	\vspace{1em} 
	\includegraphics[width=2cm]{frontmatter/licence_copyright.png}\\
	\vspace{1em} 
	\noindent Cette œuvre est mise à disposition selon les termes de la \href{http://creativecommons.org/licenses/by-sa/4.0/}{Licence Creative Commons Attribution -  Partage dans les Mêmes Conditions 4.0 International}.\\
	\vspace{1em} 
	\noindent This work is licensed under a \href{http://creativecommons.org/licenses/by-sa/4.0/}{Creative Commons Attribution-ShareAlike 4.0 International License} \\
	\vspace{1em} 
	https://creativecommons.org/licenses/by-sa/4.0/\\
 	\vspace*{\fill} 
	\end{center} 
	\newpage \rm
}

\newcommand{\frenchabstractpage}{
	\newpage
	\pagenumbering{roman}
	\setcounter{page}{3}
	\pagestyle{fancy}
		\foreignlanguage{french}{
		\lhead{Directeur de th\`{e}se: \@advisor} \rhead{\@author}}
	\renewcommand{\headrulewidth}{0.0pt} 
	\begin{center}
	\vspace*{1pt}
		\Large \foreignlanguage{french}{\@frenchtitle} \normalsize\\
	\vspace*{15pt}
		\sc \foreignlanguage{french}{R\'{e}sum\'{e}} \\ \rm
	\end{center}
	\doublespace %Harvard registrar requests that abstract is double spaced
		\foreignlanguage{french}{\input{frontmatter/frenchabstract}}
	\doublespace %Harvard registrar requests that abstract is double spaced	
	\newpage \lhead{} \rhead{}
	\cfoot{\thepage}
	\onehalfspacing
}

\newcommand{\abstractpage}{
	\cleardoublepage
	%\pagenumbering{roman}
	%\setcounter{page}{3}
		\pagestyle{fancy}
	\lhead{Thesis advisor: \@advisor} \rhead{\@author}
		\renewcommand{\headrulewidth}{0.0pt} 
		\begin{center}
		\vspace*{1pt}
	\Large \@title \normalsize\\
		\vspace*{15pt}
	\sc Abstract \\ \rm
		\end{center}
		\doublespace %Harvard registrar requests that abstract is double spaced
	\input{frontmatter/abstract}
		\doublespace %Harvard registrar requests that abstract is double spaced	
		\newpage \lhead{} \rhead{}
		\cfoot{\thepage}
		\onehalfspacing
	}


\newcommand{\dedicationpage}{
	% \pagestyle{fancy}
	\cleardoublepage \vspace*{\fill}
	\sc \noindent \input{frontmatter/dedication}
	\vspace*{\fill} \newpage \rm
	}

% the list of authors
\newcommand{\authorlist}{
	\pagestyle{fancy}
	\cleardoublepage
	\thispagestyle{fancy} 
	\chapter*{Liste des articles tirés de la thèse}
	\noindent \input{frontmatter/authorlist}
	\newpage \rm
	}

% the acknowledgments page
\newcommand{\acknowledgments}{
	\cleardoublepage
    \foreignlanguage{french}{
	\chapter{Remerciements}
	\noindent
	\input{frontmatter/thanks}
    }
	\vspace*{\fill} 
}


% the list of acronyms
\newcommand{\acronyms}{
	\pagestyle{fancy}
	\cleardoublepage
	\thispagestyle{fancy} 
	\chapter*{Acronyms}
	\noindent \input{frontmatter/acronyms}
	\newpage \rm
	}
	
\newcommand{\keywords}{
	\pagestyle{fancy}
	\cleardoublepage
	\thispagestyle{fancy} 
	\chapter*{Keywords}
	\noindent \input{frontmatter/keywords}
	\newpage \rm
	}
	
	
	\newcommand{\motscles}{
	\pagestyle{fancy}
	\cleardoublepage
	\thispagestyle{fancy} 
	\chapter*{Mots-Clés}
	\noindent \input{frontmatter/motscles}
	\newpage \rm
	}
		
		
	
% An environment for paragraph-style section
\providecommand\newthought[1]{%
   \addvspace{1.0\baselineskip plus 0.5ex minus 0.2ex}%
   \noindent\textsc{#1}}





\RequirePackage{adjustbox} %for sizing tables

%\RequirePackage{cleveref} % write "section" automatically

%\RequirePackage{colortbl} %allow to colour tables
%\newcommand\colorTable{\rowcolors{1}{white}{blue!20}}

\captionsetup[figure]{% use subfigure to confine changes to subcaptions % pour indenter les subcaptions, plus joli
	justification = centering}

\captionsetup[subfloat]{% use subfigure to confine changes to subcaptions % pour indenter les subcaptions, plus joli
	justification = centering, %RaggedRight, % Or justified, or centering, or raggedRight
	format=hang}
	



