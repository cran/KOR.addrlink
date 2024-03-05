\name{l1score}
\alias{l1score}
\title{Calculate L1-Distance Based Scores}
\description{
Reversed normalized absolute distance from zero. 
}
\usage{l1score(x)}
\arguments{
  \item{x}{A numeric vector}
}
\details{
\deqn{1 - \frac{|x|}{\text{max}\{1, |x|\}}}{1 - |x| / (max(1, |x|)}
}
\value{
A numeric vector of the same length as x
}
\author{Daniel Schürmann}

