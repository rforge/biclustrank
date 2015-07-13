\name{biclustRank:Ranking of gene expression-based biclusters using another data source}
\alias{biclustRank-package}
\alias{biclustRank}
\docType{package}
\title{
Ranking of Biclusters
}
\description{
Ranking of gene expression-based biclusters using another source of information, (e.g. the chemical structure, target predictions, HCS, etc)
This allows to further explore whether compounds in a bicluster also share other properties aside from gene expression profiles.
}
\details{
\tabular{ll}{
Package: \tab biclustRank\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-01-21\cr
License: \tab GPL3\cr
}
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Nolen Joy Perualila

Maintainer:Nolen Joy Perualila<nolenjoy.perualila@uhasselt.be>
~~ The author and/or maintainer of the package ~~
}
\references{
soon
}

\keyword{package}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
##load data
data(expressionMatrix)
data(fingerprints)

##run Fabia
	set.seed(12345)
	resFabia <- fabia(expressionMatrix, p=10, cyc=1000,  alpha=0.1, random=0)
##other options: plaid and isa

##get Fabia biclusters
	bicF <- extractBicList(data=expressionMatrix, biclustRes=resFabia, p=10, bcMethod="fabia")

## get similarity matrix for the fingerprints
fpfDist <- Distance(dataB = t(fingerprints), distmeasure = "tanimoto")
fpfSim <- 1-fpfDist

##extract similarity matrix per bc
simMat <- extractSimBC(biclustList = bicF, simMat = fpfSim, p=10 )

## get lower triangle of simMat
simVecBC <- getLowerSim(simMat)

## cdf plot
cumBC(LstTot=simVecBC, prob=TRUE, refScore=0.6)

##boxplot
boxplotBC(simVecBC=simVecBC,qVal=0.5,rank=TRUE)

##summary statistics table
statTab(simVecBC=simVecBC,ordering=4)

##top N subfeatures used for the calculation of similarity
fpfs <- sapply(1:length(bicF), function(row) choosefp(bicF,fingerprints,bcNum=row,N=100))
colnames(fpfs) <- names(bicF)

bcNum=c(4,6,7)
gComb <- unlist(sapply(bicF, "[[", 2) )
geneSim <-  cor(expressionMatrix[gComb,])^2

## heatmap of similarity scores for selected biclusters
heatmapBC(simMat=geneSim, bicRes=bicF, bcNum = bcNum, main=paste("Transcriptional Profiles Similarity"))
heatmapBC(simMat=fpfSim, bicRes=bicF, bcNum =bcNum, main=paste("Structural Profiles Similarity"))

##profiles plot of genes and heatmap of fingerprints
par(mfrow=c(2,1))
bcNum=6
ppBC(bicF,eMat=expressionMatrix, bcNum=bcNum)
heatmapBC2(fingerprints,bicF,bcNum=bcNum, N=10)

##plot loadings/scores
plotFabia(resFabia, bicF, bcNum=7, plot=2)
}
