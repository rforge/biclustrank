\name{getLowerSim}
\alias{getLowerSim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtain the lower triangle similarity scores.
}
\description{
Obtain the lower triangle similarity scores. Input for \code{boxBC} and \code{cumBC}.
}
\usage{
getLowerSim(simBC)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simBC}{
A list containing the similarity scores by bicluster as returned by \code{extractSimBC}.
}
}

\value{
A list containing a vector of pairwise compound similarity scores by BC.
}
