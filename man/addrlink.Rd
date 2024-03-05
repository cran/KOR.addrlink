\name{addrlink}
\alias{addrlink}
\title{Merge Data To Reference Index}
\description{
Takes two data.frames with address data and merges them together. 
}
\usage{
addrlink(df_ref, df_match, 
col_ref = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
col_match = c("Strasse", "Hausnummer", "Hausnummernzusatz"), 
fuzzy_threshold = 0.9, seed = 1234)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df_ref}{data.frame with address references}
  \item{df_match}{data.frame with addresses to be matched}
  \item{col_ref}{character vector of length three, naming the df_ref columns which contain the steet names, house numbers and additional letters (in that order)}
  \item{col_match}{character vector of length three, naming the df_match columns which contain the steet names, house numbers and additional letters (in that order)}
  \item{fuzzy_threshold}{The threshold used for fuzzy matching street names}
  \item{seed}{Seed for random numbers}
}
\details{
The matching is done in four stages. 

\bold{Stage 1} (qAdress = 1). This is an exact match (highest quality, qscore = 1)

\bold{Stage 2} (qAdress = 2). Exact match on street name, but no valid house 
number could be found. Be aware that random house numbers might be used. 
Consider setting your own seed. qscore indicates the match quality. 
See \code{\link{match_number}} for details.

\bold{Stage 3} (qAdress = 3). No exact match on street name could be found. 
Street names are fuzzy matched. The method "jw" (Jaro-Winkler distance) from 
package stringdist is used (see stringdist-metrics). If 1 - [Jaro-Winkler distance] 
is greater than fuzzy_threshold, a match is assumed. The highest score is 
taken and house number matching is done as outlined in Stage 2. 
qscore is fuzzy_score*[house number score]. 

\bold{Stage 4} (qAdress = 4). No match (qscore = 0)
}
\value{
A list
  \item{ret}{The merged dataset}
  \item{QA}{The quality markers (qAdress and qscore)}
}
\author{Daniel Schürmann}
\seealso{\code{\link{split_address}}, \code{\link{split_number}}}