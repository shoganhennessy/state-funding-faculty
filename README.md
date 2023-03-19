# state-faculty-composition

Senan Hogan-Hennessy, 18 August 2022

Latest revision: 17 March 2023

This is the replication package for the economics research paper ``Changes in Faculty Composition and Stagnating State Support for Higher Education,'' analysis using IPEDS and public Illinois data using *R*.
A master bash file calls all code to build analysis data from raw files, and then produce analysis files.
The replicator should expect the code to run for less than 10 minutes, using at maximum 4GB of RAM.

## Data

Folder "data/" contains multiple data sources:

- "urban-ipeds/" contains all raw data files relating to the IPED Survey, collected by the Urban Institute.
Various subfolders contain raw IPEDS files (downloaded from NCES), which supplement the Urban Institute compilation.

- "states/illinois/" contains raw data on every public university instructor in the Illinois public university system 2010-2021, downloaded from IBHED.
These data are anonymised as part of this analysis, and only the anonymised version is hosted here.
See https://salarysearch.ibhe.org/ for publically available raw data.

## Analysis

Folder "programs/" contains multiple analysis, all using the *R* language, to analyse the data.

- "run.sh" sequentially calls the relevant scripts for analyse.

- "data-build/" contains *R* scripts that build analysis data files from raw data files.

- "analysis/" contains *R* scripts that statistically analyse, producing visualisations and tables presented in the paper.

## Text

Folder "text/" contains all files regarding the final report, and write-up, of the empirical analysis.
