\name{split_address}
\alias{split_address}
\title{Split Adresses Into Street, House Number And Additional Letter}
\description{
This function takes a character vector where each element is made up from a concatenation of 
street name, house number and possibly an additional letter and splits it into its parts. 
}
\usage{split_address(x, debug = FALSE)}
\arguments{
  \item{x}{A character vector}
  \item{debug}{If true, all records will be printed to the console}
}
\details{
If the function fails, consider using \code{debug = TRUE}. This will print the record, which caused the error. 
Consider filing an issue on the linked git project (see DESCRIPTION).
}
\value{
A data.frame with three columns
  \item{Strasse}{A character column containing the extracted street names}
  \item{Hausnummer}{House number}
  \item{Hausnummernzusatz}{Additional letter}
}
\author{Daniel Schürmann}
\note{For a more advanced, general purpose solution see libpostal.}
\seealso{\code{\link{split_number}}}
\examples{
split_address(c("Teststr. 8-9 a", "Erster Weg 1-2", "Ahornallee 100a-102c"))
}
