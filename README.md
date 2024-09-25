# Less Funding, More Lecturers, and Fewer Professors

Senan Hogan-Hennessy, latest revision: 23 September 2024

[![DOI](https://zenodo.org/badge/592568213.svg)](https://zenodo.org/doi/10.5281/zenodo.11373226)

## Stagnating State Funding for Higher Education and its Effect on Faculty at US Universities

Public universities employ more lecturers and fewer professors than at any other point in the last thirty years, relative to student enrolment.
At the same time, state funding for higher education has stagnated.
This paper shows that the decline in state funding led to a substitution away from professors toward lecturers at US public universities.
Using a shift-share approach to instrument for state funding, I find that universities employ 4.4\% more lecturers per student following a 10\% funding cut.
This shift is accompanied by a reduction in assistant professors and full professors per student by 1.4\% and 1.2\%, respectively.
Incumbent professors' salaries, promotion rates, and quit rates at Illinois universities remain unaffected by funding cuts, indicating that the substitution arose from limiting the hiring of new tenure-track/tenured professors.
Stagnating state funding impacts public universities and faculty, likely contributing to the deterioration of student outcomes at public universities since the 1990s.

- [state-funding-faculty-2024.pdf](https://github.com/shoganhennessy/state-funding-faculty/blob/main/state-funding-faculty-2024.pdf) is the latest version of the working paper.
- [state-funding-faculty-presentation.pdf](https://github.com/shoganhennessy/state-funding-faculty/blob/main/state-funding-faculty-presentation.pdf) is the latest version of the associated slides.


## Replication

This folder is the replication package for the paper, with statistical analysis on IPEDS and public Illinois data using the programming language *R*, and associated packages.

A master bash file calls all code to build analysis data from raw files, produce the analysis files, and compile the final paper in one-go.

The replicator should expect the code to run for less than 10 minutes, using at maximum 4GB of RAM.

## Data

Folder "data/" contains multiple data sources:

- "urban-ipeds/" contains all raw data files relating to the IPED Survey, collected by the Urban Institute.
Various subfolders contain raw IPEDS files (downloaded from NCES), which supplement the Urban Institute compilation.

- "states/illinois/" contains raw data on every public university instructor in the Illinois public university system 2010-2021, downloaded from IBHED.
These data are anonymised as part of this analysis, and only the anonymised version is hosted here.
See https://salarysearch.ibhe.org/ for publicly available raw data.

## Analysis

Folder "programs/" contains multiple analysis, all using the *R* language, to analyse the data.

- "run.sh" sequentially calls the relevant scripts for analysis, and complete replication of the entire paper.

- "data-build/" contains *R* scripts that build analysis data files from raw data files.

- "analysis/" contains *R* scripts that statistically analyse, producing visualisations and tables presented in the paper.

## Text

Folder "text/" contains all files regarding the final report, and write-up, of the empirical analysis.
