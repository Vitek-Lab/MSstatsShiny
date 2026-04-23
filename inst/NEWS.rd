\name{MSstatsShinynews}
\alias{MSstatsShinynews}

\title{MSstatsShiny News}
\encoding{UTF-8}

\section{Changes in version 1.14.0 (2026-04-23)}{\itemize{
  \item New protein turnover workflow: a dedicated template for metabolic-labeling (heavy/light) experiments lets biologists quantify protein synthesis and degradation rates across time points. 
  \item New chemoproteomics workflow: a guided template streamlines the analysis of chemoproteomics experiments. Biologists can model dose-response relationships for each protein, visualize response curves, and download both the plots and the underlying R analysis code for reproducible reporting.
  \item Sample size and power calculation for dose-response experiments: biologists designing chemoproteomics studies can now estimate the number of biological replicates per dose needed to detect a target effect size at a desired true positive rate.
  \item Metamorpheus PTM converter: researchers using the Metamorpheus database search engine for phosphoproteomics or other post-translational modification studies can now import results directly. Modification identifiers are parsed automatically from the data and presented in a drop-down menu for easy selection.
  \item Spectronaut data quality metrics: an anomaly-score calculation flags peptide features with unusual intensity patterns in Spectronaut DIA files, helping users identify and review potential data quality issues before statistical analysis.
  \item Protein subnetwork search: users can now type any gene or protein identifier into the network visualization panel to instantly extract and display its local interaction neighborhood. An advanced-options panel provides additional filters for subnetwork expansion.
  \item Network export as HTML and PNG: the interactive protein interaction network can be exported as a self-contained HTML file for sharing or as a PNG image suitable for publications and presentations.
  \item Support for large Spectronaut uploads: the Spectronaut converter now handles large quantification files that previously caused upload failures.
  \item MSstats+ summarization option: a new protein summarization method (MSstats+) is available for label-free experiments, offering an alternative to the default MSstats algorithm.
}}

\section{Changes in version 1.12.0 (2025-10-29)}{\itemize{
  \item New network interpretation module that extracts a prior knowledge network with respect to differential analysis results
  \item Support for DIANN 2.0 quantification file input
}}

\section{Changes in version 1.0.0 (2022-11-01)}{\itemize{
  \item Official release
  \item Support for label free DDA, DIA, SRM, and PRM acquisitions
  \item Support for TMT DDA acquisition
}}

\section{Changes in version 0.99.0 (2022-09-09)}{\itemize{
  \item Submission to Bioconductor
}}

