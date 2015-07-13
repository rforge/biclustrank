###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title cdf of similarity scores
#' @param data = data used in biclustering (e.g., expression matrix)
#' @param biclustRes = biclustering output
#' @param p = number of biclusters 
#' @param simMat <- similarity matrix from getSimilarity
#' @return a list containing similarity matrix per bicluster
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

	choosefp <- function (bicRes, binMat, bcNum, N = 20,fdr=0) 
	{
		binMat <- binMat[which(rowSums(binMat) != 0 & rowSums(binMat) != 
								ncol(binMat)), ]
		cpdSet <- colnames(binMat)
		cpdComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 1) )
		
		group <- factor(ifelse(colnames(binMat) %in% cpdComb, 1, 
						2))
		pFish <- apply(binMat, 1, function(x) fisher.test(table(x, 
									group))$p.value)
		pFish <- sort(pFish)
		
if(fdr>0){	
		adjP <- p.adjust(pFish, "fdr")
		targets <- names(adjP)[which(adjP<fdr)]}
	else{
		targets <- names(pFish[1:N])}
		return(targets)
	
}


