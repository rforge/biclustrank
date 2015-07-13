\name{statTab}
\alias{statTab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get the compound similarity matrix by bicluster.
}
\description{
Table of summary statistics for the similarity scores by bicluster.
}
\usage{

statTab(simVecBC, ordering=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{simVecBC}{
List of pairwise similarity scores by bicluster as returned by getLowerSim.
}
  \item{ordering}{
Takes a value of 0 to 7 with 0 as default. This allows for the reordering of table rows according to the statistics used
with 0=no reordering, 1=mean,2=median,3=SD(standard deviation), 4=CoefVar(coefficient of variation), 5= Range, 6 = MAD(mean absolute deviation), and
7=IQR (interquartile range)
}
}

\value{
A matrix of size number of biclusters by 7 statistics allowing for numerical comparison of the distribution of biclusters' similarity scores.
}
