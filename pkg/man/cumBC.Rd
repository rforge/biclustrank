\name{cumBC}
\alias{cumBC}
\title{
Plot of cdf of similarity scores by bicluster.
}
\description{
Plot of cdf of similarity scores by bicluster.
}
\usage{cumBC(LstTot,refScore=0.5,prob=TRUE,colour,...)}

\arguments{
  \item{LstTot}{List containing vectors of similarity scores by bicluster.}

  \item{refScore}{The reference similarity score.}

  \item{prob}{Display the probability of observing a similarity score greater that the \code{refScore}. Default value is \code{TRUE}.}
  
  \item{colour}{Color of the cdf lines. A vector of length equal to number of biclusters. }

\item{...}{
Other graphical parameters.
}
}

\value{
  Plot of cdf of similarity scores and returns an ordering of biclusters according to decreasing value of (1-F(refScore)).
}

\author{Nolen Joy Perualila <nolenjoy.perualila@uhasselt.be>
}
