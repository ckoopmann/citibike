# Source Code
This directory contains the **LaTex** code used to generate the presentation slides based on the output of the R-Code.

## Contents
The repository contains the following files:
* **Presentation.tex** Main latex file defining the content of the slides to be compiled into pdf presentation.
* **.sty-files** Customizations defining appearance of different elements.
* **.tex-files** Different tables containing data to be rendered in the slides.

## Reproducibility
When reproducing the results following steps need to be followed:
* Ensure Latex-Distribution contains all packages used in main tex-file.
* Ensure all plots and tabular .tex files where generated using the R-code.
* Compile **Presentation.tex** to pdf-file from this directory.
