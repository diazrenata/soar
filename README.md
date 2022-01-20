# soar



[![DOI](https://zenodo.org/badge/351215359.svg)](https://zenodo.org/badge/latestdoi/351215359)


[![codecov](https://codecov.io/gh/diazrenata/soar/branch/main/graph/badge.svg?token=COY191G22L)](https://codecov.io/gh/diazrenata/soar)

SquareOne Analyses in R

This repository contains the functions necessary to download and process the Portal Project data in preparation for the analyses in Diaz and Ernest, "Maintenance of community function through compensation breaks down over time in a desert rodent community" (in review at *Ecology*; preprint: https://www.biorxiv.org/content/10.1101/2021.10.01.462799v1). These functions are structured as a lightweight R package (`soar`) to facilitate standardized testing and documentation, and keep functions (which are called multiple times in the paper analyses) separate from scripts (that are run once to produce specific analyses/figures). The one-time analysis and figure generation scripts, as well as the manuscript itself, are located in https://github.com/diazrenata/squareone/ (Zenodo https://doi.org/10.5281/zenodo.5544361). 

To replicate the analyses in the manuscript, 

1. Install the `soar` package, either by running `remotes::install_github("diazrenata/soar")` or by downloading this repository/Zenodo archive and installing it from source.
2. Download the repository at https://github.com/diazrenata/squareone/ (Zenodo https://doi.org/10.5281/zenodo.5544361). All data used in the paper are included in this repository; alternatively, the data can be re-downloaded from the main Portal Data repository using the functions included in this package. Analysis scripts are in the `analysis` folder as .Rmd documents. 
3. Rendering each .Rmd document (e.g. "squareone/analyses/s1_model_results") will reproduce the figures and/or tables as presented in the manuscript and supplemental materials, as formatted Word documents. Running portions of the .Rmd documents interactively allows you to work through individual analyses and explore the data more fully. Note that the .Rmd documents also contain additional documentation of the rationale for specific computational methods. These details are commented out so they do not clutter up the rendered documents, but may be of interest if you are trying to reproduce this analysis exactly. 
