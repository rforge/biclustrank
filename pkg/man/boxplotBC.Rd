\name{boxplotBC}
\alias{boxplotBC}
\title{
Boxplot of similarity scores by bicluster.
}
\description{
Boxplot of similarity scores by bicluster.
}
\usage{boxplotBC(simVecBC, qVal=0.5, rank=TRUE,...)}

\arguments{
  \item{simVecBC}{ List containing vectors of similarity scores by bicluster.}

  \item{qVal}{Quantile value used for ranking BC. Default value is \code{0.5}.}

  \item{rank}{Plot boxplots ranked according to \code{qVal}. Default value is \code{TRUE}.}
  
  \item{...}{
Other graphical parameters.
}
}
\value{
  Boxplot of similarity scores and returns the vector of quantile statistics by BC.
}

\author{Nolen Joy Perualila <nolenjoy.perualila@uhasselt.be>}

