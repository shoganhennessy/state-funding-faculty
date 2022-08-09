#!/bin/bash
## Senan Hogan-Hennessy, 17 August 2022
## Instructions file to run scripts in order, to generate analyse IPEDS data.
## "R CMD BATCH" generates .Rout files showing console output of each script.

# Build data sets of relevance
cd data-build
# Build uni-level analysis data-set from mix of raw IPEDS and Urban Inst. data
R CMD BATCH --no-save urban-ipeds-build.R
# Build individual-level analysis data-set from raw data on Illinois Profs
R CMD BATCH --no-save illinois-build.R
cd ..

# Statistical analysis code
cd analysis
# Generate graphs motivating the introduction.
R CMD BATCH --no-save ipeds-public-private.R
# Analysis and regressions at the institution-level.
R CMD BATCH --no-save ipeds-shock.R
# Analysis and regressions at the individual-level, with base-year shock IV.
R CMD BATCH --no-save individual-shock.R
# Analysis and regressions at the individual-level, with rolling shock IV.
R CMD BATCH --no-save individual-shock-rolling.R
cd ..

# Compile paper, with outputs of progams as inputs for TeX files
cd text
latexmk -pdf paper.tex
latexmk -c
