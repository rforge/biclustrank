\name{ppBC}
\alias{ppBC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of gene profiles per bicluster.
}
\description{
Plot of gene profiles per bicluster.
}
\usage{

ppBC(bicRes,eMat=expressionMatrix,bcNum,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{bicRes}{An object containing the list of biclusters returned by extractBicList. 
}
  \item{eMat}{
A microarray data.
}

  \item{bcNum}{
The bicluster number to be plotted. Select only 1 bicluster.
}

\item{...}{
Other graphical parameters.
}
}

\value{
profiles plot of genes in a bicluster across all samples with the first colored samples associated to the bicluster.
}
