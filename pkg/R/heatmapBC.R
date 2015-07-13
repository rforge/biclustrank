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
heatmapBC <-function(simMat, bicRes=bicF, bcNum=c(1,2), color,...){
	cpdComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 1) )
	grp <- as.numeric(substr(names(cpdComb),3,3))
	cpdPlot <- c(cpdComb)
	cpd2 <- cpdPlot[duplicated(cpdPlot)]
	mat <- simMat[cpdPlot,cpdPlot]
	colGroup <- grp
	fontGrp <- ifelse(cpdPlot%in%cpd2,4,1)
	x <- 1:nrow(mat)
	y <- 1:ncol(mat)
	par(mar=c(3,3,3,7), xpd=TRUE)
	color <- gray(50:0 / 50)
	image(x, y, mat ,col=color, 
			xlab=" ",
			ylab=" ",
			axes=FALSE,...
			)
	mtext(rownames(mat), side = 1, at= c(1:nrow(mat)),line=0, las=2, 
			cex=0.7, font=fontGrp, col=colGroup)
	mtext(rownames(mat), side = 4, at= c(1:nrow(mat)),line=0, las=2, 
			cex=0.7, font=fontGrp, col=colGroup)
	legend("topright", inset=c(-0.2,0), legend=c(paste("BC",bcNum,sep="")), text.col=unique(colGroup), xpd=TRUE,bty="n")
}
