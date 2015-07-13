###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title extract biclusters from fabia, isa, plaid
#' @param data = data used in biclustering (e.g., expression matrix)
#' @param biclustRes = biclustering output
#' @param p = number of biclusters 
#' @param bcMethod = biclustering method used 
#' @return a list displaying the 'p' biclusters
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

extractBicList<- function(data, biclustRes, p, bcMethod=c("Fabia","Isa", "Plaid")[1]) {
	
	bcMethod <- substring(bcMethod[1],1,1)
	
	bicList <- list()

		
	if( bcMethod=="f"| bcMethod=="F") {
				resExBFab <- extractBic(biclustRes)
		for(i in 1:p)
		{
			bicList[[i]] <- list()
		
			bicList[[i]][["samples"]] = resExBFab$bic[i,]$biypn #get compounds  per BC
			bicList[[i]][["genes"]]= resExBFab$bic[i,]$bixn #get genes per BC
#			bicFpMat[[i]] = FpF[,bicList[[i]][[1]]] # get FP for each BC
		}  
}
	else if( bcMethod=="i"| bcMethod=="I") {
		bc <-  isa.biclust(biclustRes) #convert to biclust object
		extbiclust <-  bicluster(data,bc)
		if (p > bc@Number) (stop(paste("p cannot be greater than",bc@Number)))
		
		for(i in 1:p)
		{
			bicList[[i]] <- list()
			#get compounds  per BC
			bicList[[i]][["samples"]] = colnames(extbiclust[[i]])
			bicList[[i]][["genes"]]= rownames(extbiclust[[i]]) #get genes per BC
			}  
			
	}
	
	else if ( bcMethod=="p"| bcMethod=="P") {
		extbiclust = bicluster(data, biclustRes)
		if (p > biclustRes@Number) (stop(paste("p cannot be greater than",biclustRes@Number)))
		
		for(i in 1:p)
		{
			bicList[[i]] <- list()
			bicList[[i]][["samples"]] = colnames(extbiclust[[i]]) #get compounds  per BC
			bicList[[i]][["genes"]]= rownames(extbiclust[[i]]) #get genes per BC
			}  
		
	}
	else{
		stop("Incorrect choice of bcMethod. Must be one of: Fabia, Isa or Plaid.")
	}
	names(bicList) <- paste("BC",1:p,sep="")


return(bicList)
	
}


