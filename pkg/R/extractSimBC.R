###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title extract intra-bicluster similarity scores 
#' @param data = data used in biclustering (e.g., expression matrix)
#' @param biclustRes = biclustering output
#' @param p = number of biclusters 
#' @param simMat <- similarity matrix from getSimilarity
#' @return a list containing similarity matrix per bicluster
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

extractSimBC <- function(biclustList, simMat, p) {
	
	simBC <- list()	

			for(i in 1:p){
			cBC <- 	biclustList[[i]][[1]] #get compounds  per BC
			if (length(cBC)>=2){
			simBC[[i]] <- simMat[cBC,cBC]}
		else(simBC[[i]] <- (paste("There is only 1 compound,", cBC)))
		}  
		names(simBC) <- paste("BC",1:p, sep="")
return(simBC)
	
}


