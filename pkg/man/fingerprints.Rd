\name{fingerprints}
\alias{fingerprints}
\docType{data}
\title{
Chemical structure data matrix example.
}
\description{
Chemical structure matrix used to re-rank biclusters based on gene expression data..
}
\usage{data(expressionMatrix)}
\format{The format is:
 num [1:266, 1:62] 0 0 0 0 0 0 0 0 0 0 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:266] "-2147471966" "-2146175220" "-2144940316" "-2144714452" ...
  ..$ : chr [1:62] "JnJ-xx1" "JnJ-xx2" "JnJ-xx3" "JnJ-xx4" ...
  }


\examples{
data(fingerprints)
str(fingerprints) 
}
\keyword{datasets}
