\name{expressionMatrix}
\alias{expressionMatrix}
\docType{data}
\title{
Gene expression matrix example.
}
\description{
Gene expression matrix as input data for biclustering.
}
\usage{data(expressionMatrix)}
\format{The format is:
 num [1:566, 1:62] 5.08 5.4 8.75 5.22 4.71 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:566] "SNORD115-31" "SKCG-1" "SNORA38B" "LOC100128977" ...
  ..$ : chr [1:62] "JnJ-xx1" "JnJ-xx2" "JnJ-xx3" "JnJ-xx4" ...
  }

\examples{
data(expressionMatrix)
str(expressionMatrix) 
}
\keyword{datasets}
