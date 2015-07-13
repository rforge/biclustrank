\name{Distance}
\alias{Distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute for the distance matrix of another data source.
}
\description{
Compute for the distance matrix of another data source.
}
\usage{
Distance(dataB, distmeasure="tanimoto")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataB}{
Another data source not used in biclustering (similarity of rows)
}
  \item{distmeasure}{
Distance measure used in the calculation of pairwise distance. There are only three possible options: tanimoto, jaccard, or euclidean.
}
}
\value{
A list containing the similarity matrix of compounds in the bicluster.
}

\keyword{distance}
\keyword{tanimoto}% __ONLY ONE__ keyword per line
