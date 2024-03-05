\name{helper_split_number}
\alias{helper_split_number}
\title{Splits A Single House Number Into House Number And Additional Letter}
\description{This is an internal function. Please use \code{\link{split_number}}}
\usage{helper_split_number(x, debug = FALSE)
}
\arguments{
  \item{x}{A character vector of length 1}
  \item{debug}{If true, print(x)}
}
\value{
A data.frame with two elements
  \item{Hausnummer}{Extracted house number}
  \item{Zusatz}{Extracted extra letter}
}
\author{Daniel Schürmann}
\seealso{\code{\link{split_number}}}

