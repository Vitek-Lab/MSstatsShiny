# MSstatsShiny

This repository contains the code for the R Shiny app MSstatsShiny, which 
utilizes MSstats, MSstatsTMT, and MSstatsPTM to analyze proteomics experiments.

## Availability

The application is available both online and locally, via Bioconductor or Github.

### Online

The online application is located at 
[http://www.msstatsshiny.com/](http://www.msstatsshiny.com/). The online version
is constrained to processing only input files smaller than 100 MB. Due to this, 
we recommend processing large datasets using a local installation.


### Bioconductor

For large file processing, it is recommend you use a local install of the application. The easiest way to install the application locally is via [Bioconductor](https://bioconductor.org/packages/release/bioc/html/MSstatsShiny.html)

### Local

To install the application via Github, please use the following steps.

1. Download [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/products/rstudio/download/) - [How to](https://rstudio-education.github.io/hopr/starting.html). **Note R version must be >= 4.2**
2. Install the dependencies by running the following commands:
`install.packages(c('shiny', 'shinyBS', 'shinybusy', 'shinyjs', 'uuid', 'DT', 'knitr', 'plotly', 'ggrepel', 'gplots', 'tidyverse', 'data.table', 'BiocManager', 'devtools'))`
`BiocManager::install(c('MSstatsPTM', 'biomaRt'))`
3. Install the package by executing `devtools::install_github("Vitek-Lab/MSstatsShiny")` in the console.
4. Run the application by executing `library(MSstatsShiny)` and `launch_MSstatsShiny()` or `MSstatsShiny::launch_MSstatsShiny()` in the console.

## Processing instructions

Instructions for processing your dataset are listed directly in the application. The general workflow includes: uploading your data into the application, run level summarization and vizualization, and data modeling to determine differential proteins. For questions on how to process your data please see the Help section of the application.
