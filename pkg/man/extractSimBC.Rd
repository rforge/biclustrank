\name{extractSimBC}
\alias{extractSimBC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the compound similarity matrix by bicluster.
}
\description{
 Get the compound similarity matrix by bicluster.
}
\usage{
extractSimBC(biclustList, simMat, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{biclustList}{
List of bicluster returned by
  the extractBicList function
}
  \item{simMat}{
Similarity Matrix of all compounds.
}

  \item{p}{
number of biclusters
}
}

\value{
A list containing the similarity matrix of compounds in the bicluster.
}

