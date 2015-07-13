\name{heatmapBC}
\alias{heatmapBC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heatmap of similarity scores by BC.
}
\description{
Heatmap of samples' similarity scores by BC.
}
\usage{
heatmapBC(simMat, bicRes=bicF, bcNum=c(1,2), color,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simMat}{
Similarity matrix of all samples. 
}

  \item{bicRes}{
Object returned by \code{extractBicList}.
}

  \item{bcNum}{
A vector containing the bicluster number(s) to be plotted.
}

\item{color}{
Heatmap color, default to gray pallette.
}

\item{...}{
Other graphical parameters.
}
}

\value{
heatmap of the similarity scores of combined samples from blusters \code{bcNum}.
}
