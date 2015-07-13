###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title boxplot of similarity scores by BC
#' @param data = data used in biclustering (e.g., expression matrix)
#' @param biclustRes = biclustering output
#' @param p = number of biclusters 
#' @param simMat <- similarity matrix from getSimilarity
#' @return a list containing similarity matrix per bicluster
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

statTab <- function(simVecBC, ordering=0)
{
	#simList <- lower triangle of simMat 
	simList <- simVecBC

	mean <-  unlist(lapply(simList, FUN=mean ))
	SD <- unlist(lapply(simList, FUN=sd ))
	CoefVar <- unlist(lapply(simList, FUN=function(x) round((sd(x)/mean(x))*100,2) ))
	median <- unlist(lapply(simList, FUN=median ))
	Range <- unlist(lapply(simList, FUN=function(x) max(x)-min(x)))
	IQR <- unlist(lapply(simList, FUN=IQR ))
	MAD <- unlist(lapply(simList, FUN=mad ))
	
	datStat0 <- round(cbind(mean,median, SD, CoefVar, MAD, Range, IQR),2)
	
	if(ordering!=0){
		
	s <- c("mean","median", "SD", "CoefVar",  "MAD", "Range", "IQR")
	if(ordering>=3){
	namesBC <- rownames(datStat0)[order(get(s[ordering]), decreasing=FALSE)]
}
else{
	namesBC <- rownames(datStat0)[order(get(s[ordering]), decreasing=TRUE)]
}
	datStat <- datStat0[namesBC,]
}
else{
	datStat <- datStat0
}

return(datStat)
}