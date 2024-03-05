\name{helper_split_address}
\alias{helper_split_address}
\title{Splits A Single Address Into Street, House Number And Additional Letter}
\description{This is an internal function. Please use \code{\link{split_address}}}
\usage{helper_split_address(x, debug = FALSE)
}
\arguments{
  \item{x}{A character vector of length 1}
  \item{debug}{If true, print(x)}
}
\value{
A list with three elements
  \item{strasse}{Extracted street name}
  \item{hnr}{Extracted house number}
  \item{hnrz}{Extracted extra letter}   
}
\author{Daniel Schürmann}
\seealso{\code{\link{split_address}}}
