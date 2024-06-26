% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combination_run.R
\name{sanitize_batch}
\alias{sanitize_batch}
\title{Run function}
\usage{
sanitize_batch(
  path,
  number = 1,
  recovery = FALSE,
  where_to_save = NULL,
  study_batch = 2
)
}
\arguments{
\item{path}{Mandatory\cr
path where real data located, should be directory}

\item{number}{mandatory, default 1\cr
how many studies to generate}

\item{recovery}{optional\cr
recovery}

\item{where_to_save}{optional\cr
if no directory path given, fake/generate study data will be save
directory given in path argument}

\item{study_batch}{mandatory, fefault 2\cr
interger value}
}
\description{
Run function
}
