###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title heatmap of similarity scores by BC
#' @param simVecBC = list containing vectors of similarity scores by bicluster
#' @param qval = quantile value
#' @param rank = logical, rank boxplots by quantile statistics
#' @param ... other plot parameters
#' @return boxplot of similarity scores and display of quantile statistics 
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)
heatmapBC2 <-function(mat, bicRes=bicF, bcNum=1, N=10, col,...){
	cpdComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 1) )
	gComb <-choosefp(bicRes,mat,bcNum,N=N)
	grp <- as.numeric(substr(names(cpdComb),3,3))
	ordCMPDS <- c(cpdComb, colnames(mat)[!colnames(mat)%in%cpdComb])
	colorlead <- ifelse(ordCMPDS%in%cpdComb, bcNum, "black")
	mat <- t(mat[rev(gComb),ordCMPDS])
	colBC <- grp
	x <- 1:nrow(mat)
	y <- 1:ncol(mat)
	par(mar=c(7,4,2,3), xpd=TRUE)
	image(x, y, mat, col=c('gray90','blue'),
			xlab=" ",
			ylab=" ",
			axes=FALSE,
			...)
	mtext(rownames(mat), side = 1, at= c(1:nrow(mat)),line=0, las=2, 
			cex=0.8, col=colorlead)
	mtext(colnames(mat), side = 2, at= c(1:ncol(mat)),line=0, las=2, 
			cex=0.8)
	mtext("Compounds", side = 1,  line=3, las=0, 
			cex=1)
	mtext("Structures", side = 4,  line=1, las=0, 
			cex=1)
}