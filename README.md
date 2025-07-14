# README

This repository contains the code for the R Shiny app MSstatsShiny, which utilizes MSstats, MSstatsTMT, and MSstatsPTM to analyze proteomics experiments.

# MSstatsShiny Tutorial

This tutorial will walk through the steps on MSstatsShiny for performing differential abundance analysis for a dataset from Fragpipe. We use a case study of a DIA experiment that was analyzed using the FragPipe computational tool. The dataset originates from a clear cell renal cell carcinoma (ccRCC) study described in this paper. In the original study, researchers from the CPTAC profiled tumor (T) samples, together with normal adjacent tissue (NAT) samples from each cancer patient, indicating a paired design.

## Installation

All datasets for this tutorial can be found at this [link](https://github.com/Vitek-Lab/MSstatsShiny/tree/devel/inst/extdata/tutorial)

Install MSstatsShiny using the instructions below:

-   Download R - <https://cran.r-project.org/>. Note R version must be \>= 4.4

    -   Note: if on windows you must also install R Tools - <https://cran.r-project.org/bin/windows/Rtools/>

    -   Optionally you can also install RStudio Desktop - <https://posit.co/downloads/>

-   Run R or RStudio (if downloaded)

-   In the console run the installation code (see below) (<https://bioconductor.org/packages/release/bioc/html/MSstatsShiny.html>)

```{r}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("MSstatsShiny")
```

-   If this does not work due to a release 3.21 issue, you can install release 3.20 with the following 2 commands:

```{r}
BiocManager::install(version = '3.20', force = TRUE)
BiocManager::install('MSstatsShiny', version = '3.20', force = TRUE)
```

-   If you get a bug related to lme4, you can install the latest version of lme4 with the following command:

```{r}
install.packages("lme4", type = "source")
```

-   You can also install from Github with the following command:

```{r}
devtools::install_github("Vitek-Lab/MSstatsShiny", build_vignettes = TRUE)
```

-   MSstatsShiny can now be started by running `MSstatsShiny::launch_MSstatsShiny()` in the console

### Online

The online application is located at <http://www.msstatsshiny.com/>. The online version is constrained to processing only input files smaller than 100 MB. Due to this, we recommend processing large datasets using a local installation.

### Load Package

```{r}
library(MSstatsShiny)
```

## Step 1: Launch MSstatsShiny

```{r}
MSstatsShiny::launch_MSstatsShiny()
```

Click the `Run MSstats Pipeline` button to move to the data upload step. 

## Step 2: Data Upload

### Biological Question & Data Type

-   Biological Question:

    -   Protein: Differential abundance analysis of proteins across conditions.

    -   Peptide: Differential abundance analysis of specific peptides across conditions.

    -   PTM: Differential abundance analysis of PTMs across conditions.  See this [paper](https://www.mcponline.org/article/S1535-9476(22)00285-7/fulltext) for more information on how MSstatsPTM performs differential abundance analysis of PTMs via bottom-up MS proteomics.

-   Label Type:

    -   Label-Free: Default setting associated with label-free DDA, DIA, SRM/MRM, PRM experiments

    -   TMT: Use if your experiment uses tandem mass tags to perform sample multiplexing.

For this dataset, keep the default setting of `Protein` for the biological question and `Label-Free` for the label type.

### Attach Quantification Dataset

Each tool produces a quantification report that can be uploaded to MSstatsShiny to begin data conversion. For example, Fragpipe has this [tutorial](https://fragpipe.nesvilab.org/docs/tutorial_msstats.html) where you can export your quantifications in the format of MSstats.

Upload `msstats.csv` from this [link](https://github.com/Vitek-Lab/MSstatsShiny/tree/devel/inst/extdata/tutorial) as the quantification dataset. Keep the default setting of `comma` for column separator.

### Upload Annotation File

The annotation file defines the experimental design, notably which BioReplicate and Condition are associated with a particular MS run. You can see `annotation.csv` from this [link](https://github.com/Vitek-Lab/MSstatsShiny/tree/devel/inst/extdata/tutorial) as an example of an annotation file.

Keep in mind that we must indicate that this is a paired design to MSstats. To do this, each pair of runs that corresponds to the same bioreplicate should be assigned the same ID in the BioReplicate column. In a standard group comparison design, each bioreplicate will have a unique ID. The image below illustrates how the annotation file is set up for paired designs.

### Select the options for pre-processing

-   Use unique peptides: If enabled, MSstats will remove any peptides that match with multiple proteins. Keep this option enabled.

-   Remove proteins with 1 peptide and charge: If enabled, MSstats will remove any proteins that have only 1 peptide quantified across all runs. We won’t enable this for now.

-   Remove proteins with 1 feature: If enabled, MSstats will remove any proteins that have only 1 peptide spectral match across all runs. We won’t enable this for now.

After attaching the dataset, the upload button should be enabled.

### Output

You should see a summary of your dataset and the top 6 rows of your dataset.  Click `Next step` to proceed to data processing.

## Citation

To cite this application please use the corresponding publication in the journal of proteome research.

**MSstatsShiny: A GUI for Versatile, Scalable, and Reproducible Statistical Analyses of Quantitative Proteomic Experiments**

Devon Kohler, Maanasa Kaza, Cristina Pasi, Ting Huang, Mateusz Staniak, Dhaval Mohandas, Eduard Sabido, Meena Choi, and Olga Vitek. Journal of Proteome Research 2023 22 (2), 551-556 DOI: 10.1021/acs.jproteome.2c00603
