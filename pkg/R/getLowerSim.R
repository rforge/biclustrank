###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title lower triangle similarity scores
#' @param simBC = list containing matrix of simialrity scores
#' @return a list containing vector of pairwise compound similarity scores by BC
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)


getLowerSim <- function(simBC){
	getLowerTri <- function(x){(x[lower.tri(x)])}
	simL <- lapply(simBC,getLowerTri)
	getBC <- which(lapply(simL,length)>=2)
	simVec <- simL[getBC]
	names(simVec) <- paste("BC",getBC,sep="")
	return(simVec)
}
