# An open-source PhD Template

[![License](https://img.shields.io/badge/license-AGPL-FBB829.svg)](https://www.gnu.org/licenses/agpl-3.0.html)

Works with MAC, Windows and Linux.

Get your PhD ready in five simple steps:
1. Download all the files and unzip them (or clone the repository).
2. Install the fonts. They are all in the "font" folder. Select all of them, right-click, and click "Install".
3. Put your .tex and .bib files in the "chapter" folders. If you want to add more chapters, just create new folders and edit the dissertation.tex file to add new entries.
4. Put all your figures in the "figures" folder.
5. Run the file dissertation.tex.

[Here is an example](https://s3.eu-central-1.amazonaws.com/qperrier/dissertation.pdf).

To compile, use xelatex (not pdflatex) and biber (not bibtex). These are easily configurable with the IDE [TexStudio](http://www.texstudio.org). 
For bibliography, this class uses biblatex (not natbib). As usual, to display bibliography properly, run the .tex file, then run Bibliography to generate the .bbl file, then run the .tex twice. 

Acknoledgments: this work was built from the template made by [Jordan Suchow](https://github.com/suchow/Dissertate).

