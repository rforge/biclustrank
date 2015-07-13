\name{plotFabia}
\alias{plotFabia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of the gene loadings and/or sample scores by FABIA bicluster.
}
\description{
Plot of the gene loadings and/or sample scores by FABIA bicluster.
}
\usage{
plotFabia(resFabia, bicRes, bcNum, plot=c(1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resFabia}{
Biclustering result returned by \code{fabia}.
}

  \item{bicRes}{
Object returned by \code{extractBicList}.
}

  \item{bcNum}{
A vector containing the bicluster number to be plotted. Select only 1 bicluster.
}

\item{plot}{
Indicating which plot to show: 1= loadings, 2= sample. Select only one.
}

}

\value{
Plot of loadings(scores) highlighting the gene(samples) that comprises the bicluster.
}
