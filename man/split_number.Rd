\name{split_number}
\alias{split_number}
\title{Split house number into house number and additional letter}
\description{
This function takes a character vector where each element is made up from a concatenation of 
house number and possibly an additional letter and splits is into its parts. 
}
\usage{split_number(x, debug = FALSE)}
\arguments{
  \item{x}{A character vector}
  \item{debug}{If true, all records will be printed to the console}
}
\details{
If the function fails, consider using \code{debug = TRUE}. This will print the record, which caused the error. 
Consider filing an issue on the linked git project (see DESCRIPTION).
}
\value{
A data.frame with two columns
  \item{Hausnummer}{House number}
  \item{Hausnummernzusatz}{Additional letter}
}
\author{Daniel Schürmann}
\note{For a more advanced, general purpose solution see libpostal.}
\seealso{\code{\link{split_address}}}
\examples{
split_number(c("8-9 a", "1-2", "100a-102c"))
}
