\name{choosefp}
\alias{choosefp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Select top features that differentiates the samples in the bicluster from the other samples. 
}
\description{
Select top features that differentiates the samples in the bicluster from the other samples. 
}
\usage{
choosefp(bicRes, binMat, bcNum, N = 20,fdr=0) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{bicRes}{An object containing the list of biclusters returned by extractBicList. 
}
  \item{binMat}{
Binary matrix of chemical structures that is used to rank the biclusters.
}

  \item{bcNum}{
The bicluster number of interest. Select only 1 bicluster.
}

\item{N}{
The number of top features to be identified.
}

\item{fdr}{
False discovery rate used to identify significant features from the univariate fisher's exact test 
 If this is given, N is ignored. The default is 0, making use of N.
}

}

\value{
Gives a subset of the row names of \code{mat}.
}

\keyword{ fdr }

