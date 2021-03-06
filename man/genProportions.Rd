% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateData.r
\name{genProportions}
\alias{genProportions}
\title{genProportions}
\usage{
genProportions(ncol, nrow, var.names = NULL)
}
\arguments{
\item{ncol}{numeric}

\item{nrow}{numeric}

\item{var.names}{a vector of strings}
}
\description{
function to generate random proportions whose rowSums = 1
}
\examples{
genProportions(ncol=5, nrow=5)
genProportions(ncol=3, nrow=25)
genProportions(ncol=3, nrow=5, var.names=c("red", "blue", "green"))
}
