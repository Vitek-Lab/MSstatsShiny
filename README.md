# MSstatsShiny

This repository contains the code for the R Shiny app MSstatsShiny, which utilizes MSstats, MSstatsTMT, and MSstatsPTM to analyze proteomics experiments.

## MSstatsShiny Tutorial

This tutorial will walk through the steps on MSstatsShiny for performing differential abundance analysis for a dataset from Fragpipe. We use a case study of a DIA experiment that was analyzed using the FragPipe computational tool. The dataset originates from a clear cell renal cell carcinoma (ccRCC) study described in this paper. In the original study, researchers from the CPTAC profiled tumor (T) samples, together with normal adjacent tissue (NAT) samples from each cancer patient, indicating a paired design.

### Installation

All datasets for this tutorial can be found at this link (tbd: insert github link)

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

#### Online

The online application is located at <http://www.msstatsshiny.com/>. The online version is constrained to processing only input files smaller than 100 MB. Due to this, we recommend processing large datasets using a local installation.

### Load Package

```{r}
library(MSstatsShiny)
```

### Launch MSstatsShiny

```{r}
MSstatsShiny::launch_MSstatsShiny()
```

Click the `Run MSstats Pipeline` button to move to the data upload step. 

## Citation

To cite this application please use the corresponding publication in the journal of proteome research.

**MSstatsShiny: A GUI for Versatile, Scalable, and Reproducible Statistical Analyses of Quantitative Proteomic Experiments**

Devon Kohler, Maanasa Kaza, Cristina Pasi, Ting Huang, Mateusz Staniak, Dhaval Mohandas, Eduard Sabido, Meena Choi, and Olga Vitek. Journal of Proteome Research 2023 22 (2), 551-556 DOI: 10.1021/acs.jproteome.2c00603
