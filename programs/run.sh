#!/bin/bash
## Senan Hogan-Hennessy, 17 August 2022
## Instructions file to run scripts in order, to generate analyse IPEDS data.

# Build data sets of relevance
cd data-build
# Build uni-level analysis data-set from mix of raw IPEDS and Urban Inst. data
Rscript urban-ipeds-build.R
# Build individual-level analysis data-set from raw data on Illinois Profs
Rscript illinois-build.R
cd ..

# Statistical analysis code
cd analysis
# Generate graphs motivating the introduction.
Rscript ipeds-public-private.R
# Analysis and regressions at the institution-level.
Rscript ipeds-shock.R
# Analysis and regressions at the individual-level, with base-year shock IV.
Rscript individual-shock.R
# Analysis and regressions at the individual-level, with rolling shock IV.
Rscript individual-shock-rolling.R
cd ..

# Compile paper, with outputs of progams as inputs for TeX files
cd text
latexmk -pdf paper.tex
latexmk -c
rm paper.bbl
