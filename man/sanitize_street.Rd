\name{sanitize_street}
\alias{sanitize_street}
\title{Clean Steet Names And Make Them Mergeable}
\description{
This function replaces Umlauts, expands "str" to "strasse", 
transliterates all non-ascii characters, removes punctuation and converts 
to lower case.
}
\usage{sanitize_street(x)}

\arguments{
  \item{x}{A character vector containing the steet names}
}
\details{
This is an internal function used in \code{addrlink}. 
Make sure house numbers have already been extracted. 
Use \code{split_number} or \code{split_address} for that. 
Only steet names can go into \code{sanitize_street}. 
}
\value{
A character vector of the same length as x containing the 
sanitized street names. }
\author{Daniel Schürmann}

\seealso{
\code{\link{split_address}}, \code{\link{split_number}}, \code{\link{addrlink}}
}

