\name{heatmapBC2}
\alias{heatmapBC2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Heatmap of the chemical structure data using the top features by BC.
}
\description{
Heatmap of the chemical structure data using the top features by BC.
}
\usage{
heatmapBC2(mat, bicRes=bicF, bcNum=1, N=10, col,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
data matrix with features as rows and samples as columns. 
}

  \item{bicRes}{
Object returned by \code{extractBicList}.
}

  \item{bcNum}{
The bicluster number to be plotted. Select only 1 bicluster.
}

  \item{N}{
The number of top differentiating features to be plotted.
}
\item{col}{
Heatmap color
}

\item{...}{
Other graphical parameters.
}
}
\value{
heatmap of the top chemical structures for bicluster \code{bcNum}.
}
