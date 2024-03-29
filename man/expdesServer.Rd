% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/module-expdes-server.R
\name{expdesServer}
\alias{expdesServer}
\title{Expdes Server module for future experiments}
\usage{
expdesServer(
  input,
  output,
  session,
  parent_session,
  loadpage_input,
  qc_input,
  statmodel_input,
  data_comparison
)
}
\arguments{
\item{input}{input object to capture different ui element values}

\item{output}{to render and create elements}

\item{session}{session current module}

\item{parent_session}{session of the main calling module}

\item{loadpage_input}{input object from loadpage UI}

\item{qc_input}{input object from QC UI}

\item{statmodel_input}{input object from Statmodel UI}

\item{data_comparison}{function for group comparisons}
}
\value{
list object with user selected options and matrix build
}
\description{
This function sets up the Expdes server to process data based on user
selected inputs
}
\examples{
NA

}
