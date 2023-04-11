## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=8
)

## ---- eval = FALSE------------------------------------------------------------
#  if (!requireNamespace("BiocManager", quietly = TRUE))
#      install.packages("BiocManager")
#  
#  BiocManager::install("MSstatsShiny")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(MSstatsShiny)

## ---- eval = FALSE, message=FALSE, warning=FALSE------------------------------
#  launch_MSstatsShiny()

## -----------------------------------------------------------------------------
sessioninfo::session_info()

