% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_functions.R
\name{replaceNested}
\alias{replaceNested}
\title{Replace nested brackets in a pedigree name
A function to replace pairs of nested brackets (squire and open) to establish breeding generation in a pedigree}
\usage{
replaceNested(x, left = NA, right = NA, replace = "??")
}
\arguments{
\item{x}{the pedigree or name string}

\item{left}{A character symbol for type of left bracket}

\item{right}{A character symbol for the type of right bracket}

\item{replacement}{String or symbol for replacing the string between the left and right bracket}
}
\description{
replaceNested
}
\keyword{nomenclature}
