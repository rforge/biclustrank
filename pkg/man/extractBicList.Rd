\name{extractBicList}
\alias{extractBicList}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extracting biclusters from the results of running fabia, isa, plaid.
}
\description{
This is used to extract the labels of samples and variables from a biclustering result using either fabia, plaid, isa.
}
\usage{
extractBicList(data, biclustRes, p, bcMethod=c("Fabia","Isa", "Plaid")[1])
}

\arguments{
  \item{data}{data used in biclustering (e.g., expression matrix).
}
  \item{biclustRes}{Biclustering output. 
}
  \item{p}{Number of biclusters, default is 10.
}
  \item{bcMethod}{Biclustering method used, c("fabia", "plaid","isa").
}

}

\value{
A list of length 2 displaying the following objects for each \code{p} biclusters.
 \item{samples}{sample names}
  \item{variables }{variable names}
}

\keyword{fabia}
\keyword{plaid}
