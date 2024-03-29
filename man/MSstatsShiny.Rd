% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MSstatsShiny.R
\docType{package}
\name{MSstatsShiny}
\alias{MSstatsShiny-package}
\alias{MSstatsShiny}
\title{MSstatsShiny: An R-shiny based package for detecting differencially abundant
proteins, integrated with the MSstats family of packages.}
\description{
A set of tools for detecting differentially abundant proteins in
shotgun mass spectrometry-based proteomic experiments. The package can handle
a variety of acquisition types, including label free, DDA, DIA, and TMT. The
package includes tools to convert raw data from different spectral processing
tools, summarize feature intensities, and fit a linear mixed effects model.
The GUI supports different biological queries including those targeting the
global proteome and post translational modifications. Additionally the
package includes functionality to plot a variety of data visualizations.
}
\section{functions }{

\itemize{
\item \code{\link{launch_MSstatsShiny}} : Main function to launch the
application.
\item \code{\link{groupComparisonPlots2}} : Generates MSstatsShiny plots.
\item \code{\link{lf_summarization_loop}} : Summarization for LF
experiments.
\item \code{\link{tmt_summarization_loop}} : Summarization for TMT
experiments.
\item \code{\link{lf_model}} : Modeling for LF experiments.
\item \code{\link{tmt_model}} : Modeling for TMT experiments.
}
}

\seealso{
Useful links:
\itemize{
  \item Report bugs at \url{https://github.com/Vitek-Lab/MSstatsShiny/issues}
}

}
\author{
\strong{Maintainer}: Devon Kohler \email{kohler.d@northeastern.edu}

Authors:
\itemize{
  \item Deril Raju \email{raju.d@northeastern.edu}
  \item Maanasa Kaza \email{maanasakaza@gmail.com}
  \item Cristina Pasi \email{cristinapasi@gmail.com}
  \item Ting Huang \email{thuang0703@gmail.com}
  \item Mateusz Staniak \email{mateusz.staniak@math.uni.wroc.pl}
  \item Dhaval Mohandas \email{dhavalmohandas@gmail.com}
  \item Eduard Sabido \email{eduard.sabido@crg.cat}
  \item Meena Choi \email{choi.meena@gene.com}
  \item Olga Vitek \email{o.vitek@northeastern.edu}
}

}
