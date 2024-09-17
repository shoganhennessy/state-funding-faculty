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
# Build uni-level analysis data-set from open-sorce data on hiring networks.
R CMD BATCH --no-save hiring-build.R
cd ..

# Statistical analysis code
cd analysis
# Generate graphs motivating the introduction.
R CMD BATCH --no-save ipeds-public-private.R
# Analysis and regressions at the institution-level, IPEDS
R CMD BATCH --no-save ipeds-shock.R
# Analysis and regressions at the institution-level, IPEDS, with count outcomes.
R CMD BATCH --no-save ipeds-shock-rawcounts.R
# Robustness checks at the institution-level, IPEDS
R CMD BATCH --no-save ipeds-robustness-checks.R
# Analysis and regressions at the institution-level, IPEDS, local porjections.
R CMD BATCH --no-save ipeds-shock-longterm.R
# Analysis and regressions at the individual-level, IBHED, base-year shock IV.
R CMD BATCH --no-save ibhed-shock.R
# Analysis and regressions at the using IBHED count outcomes, instituion level.
R CMD BATCH --no-save ibhed-shock-rawcounts.R
# Analysis and regressions at the individual-level, IBHED, rolling shock IV.
R CMD BATCH --no-save ibhed-shock-rolling.R
# Analysis and regressions at the institution-individual-level, IBHED, local porjections.
R CMD BATCH --no-save ibhed-shock-longterm.R
# Analysis and regressions for rate of faculty hiring.
R CMD BATCH --no-save hiring-shock.R
cd ..

# Compile paper, with outputs from R scripts above as inputs for TeX files
cd ../text
latexmk -pdf paper.tex
latexmk -c

# Host the draft as the main file in the replication package.
cp paper.pdf ../state-funding-faculty-2024.pdf

# Put the most recent presentation into the main folder.
cd ../presentation
cp presentation.pdf ../state-funding-faculty-presentation.pdf
