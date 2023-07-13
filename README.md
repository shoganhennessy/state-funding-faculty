# state-funding-faculty

Senan Hogan-Hennessy, first draft on 18 August 2022.

Latest revision: 5 June 2023

This is the replication package for the economics research paper ``Stagnating State Funding for Higher Education and its Effect on Faculty at US Universities,'' with statistical analysis on IPEDS and public Illinois data using the programming language *R*, and associated packages.
A master bash file calls all code to build analysis data from raw files, produce the analysis files, and compile the final paper in one-go.

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

- "run.sh" sequentially calls the relevant scripts for analysis, and complete replication of the entire paper.

- "data-build/" contains *R* scripts that build analysis data files from raw data files.

- "analysis/" contains *R* scripts that statistically analyse, producing visualisations and tables presented in the paper.

## Text

Folder "text/" contains all files regarding the final report, and write-up, of the empirical analysis.

- "state-funding-faculty-2023.pdf" is the latest version of the working paper.

## July to do (ordered):

### Re-writing

Pitch differently, following Evan's guide.

- Re-write introduction, from the ground up, and following sections with a focus on mechanisms.  Add a data section in introduction.
- Add into a footnote that compliers are likely every (public) university, which can be mentioned in the discussion when a comparison to private unis.
- Print out, and make notes for writing (topic sentences to start sections + paragraphs).
- Add mention of tenure + free speech
https://www.aeaweb.org/articles?id=10.1257/jep.13.1.85
https://www.sciencedirect.com/science/article/abs/pii/0167268183900057
https://doi.org/10.1016/0167-2681(92)90002-S

2. Adjust the LaTeX tables, by hand, to follow the ``large and fill page'' tuning.
See Table 5 of [this](https://doi.org/10.1162/rest_a_01098) for a publication-ready table, similar form to mine.
