\name{match_number}
\alias{match_number}
\title{Find Best House Number Match Within Given Street}
\description{This is an internal function. Please use \code{\link{addrlink}}}
\usage{match_number(record, Adressen, weights = c(0.9, 0.1))}

\arguments{
  \item{record}{data.frame with one row and three columns (Strasse, Hausnummer, Hausnummernzusatz)}
  \item{Adressen}{data.frame of all valid addresses (same columns as record data.frame)}
  \item{weights}{The weighing factors between house number and additional letter}
}

\details{
If no house number and no additional letter is provided, a random address in 
the given street is selected (qscore = 0).

If only an additional letter but no house number is given and the letter is unique, 
returns the corresponding record (qscore = 0.05). Otherwise returns a random one 
as mentioned above (qscore = 0). 

If no additional letter, but house number is provided and the maximum distance to 
a valid house number is 4, return the closest match as calculated by 
\code{\link{l1score}} (qscore is the result of l1score). Otherwise a random record 
is returned (qscore = 0). 

If additional letter and house number are available and the house number distance 
is smaller then 4, calculates the l1scores of the house number distance and 
addional letters distance and selects the best match (qscore is the sum of both 
weighted l1scores). Otherwise a random record is selected (qscore = 0). 
}
\value{
A data.frame
  \item{qscore}{The quality score of the match}
  \item{Strasse}{matched street}
  \item{Hausnummer}{matched house number}
  \item{Hausnummernzusatz}{matched additional letter}
}
\author{Daniel Schürmann}
\seealso{\code{\link{addrlink}}}