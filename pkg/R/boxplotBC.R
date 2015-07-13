###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title boxplot of similarity scores by BC
#' @param simVecBC = list containing vectors of similarity scores by bicluster
#' @param qval = quantile value
#' @param rank = logical, rank boxplots by quantile statistics
#' @param ... other plot parameters
#' @return boxplot of similarity scores and display of quantile statistics 
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

boxplotBC <- function(simVecBC, qVal=0.5, rank=TRUE,...)
{
	#simList <- lower triangle of simMat 
	simList <- simVecBC
	
	group <- list()
	bcNames <- names(simList)
	for (i in 1:length(simList)){
			group[[i]] <- rep(bcNames[i], length(simList[[i]])) 
		
	}
#		myskewness <-  function(x) {
#			m3 <- mean((x-mean(x))^3)
#			skew <- m3/(sd(x)^3)
#			skew
#		}
	myQuantile <- function(x){
		quantile90 <- quantile(x,qVal)
		quantile90
	}
	statOrd <-  unlist(lapply(simList, FUN=myQuantile ))
names(statOrd) <- bcNames
	namesBC <- bcNames[order(statOrd, decreasing=TRUE)]
	statOrd <- statOrd[namesBC]
	group <- unlist(group)
	
	dataMat <- data.matrix(cbind(unlist(simList),group))
	if(rank)
	{
		dataMat[,2] <- ordered(dataMat[,2], levels=(namesBC))
		qval2 <- qVal*100
		boxplot(as.numeric(dataMat[,1])~factor(dataMat[,2]),  col="gray", 
				main = paste("  Biclusters Ranked in Descending Order of  \n", qval2,"th Percentile Similarity Score", sep=""), xlab=" ", 
				ylab ="similarity scores", names=namesBC, outline=TRUE,...)
		points( (as.numeric(dataMat[,2] )),
				as.vector(as.numeric(dataMat[,1])), cex=1, pch=21, bg="gray90")
		stat <- (round(statOrd[namesBC],2))
	}
	else{
		dataMat[,2] <- ordered(dataMat[,2], levels=(bcNames))
	boxplot(as.numeric(dataMat[,1])~factor(dataMat[,2]),  col="gray", 
			main = "Boxplot of Compound Similarity Scores by bicluster", xlab=" ", 
			ylab ="similarity scores", names=bcNames,outline=TRUE,...)
	points( (as.numeric(dataMat[,2] )),
			as.vector(as.numeric(dataMat[,1])), cex=1, pch=21, bg="gray90")
	
	stat <- (round(statOrd[bcNames],2))
}
return(stat)
}