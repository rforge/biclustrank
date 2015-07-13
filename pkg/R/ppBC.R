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

ppBC <-
		function(bicRes,eMat=expressionMatrix,bcNum,...)
{
	cpdComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 1) )
	gComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 2) )
	
	eMatPlotC <- t(apply(eMat,1, function(c) c-mean(c)))
	ordCMPDS <- c(cpdComb, colnames(eMat)[!colnames(eMat)%in%cpdComb])
	plotData <- eMatPlotC[gComb[1:10], ordCMPDS]
	colorlead <- ifelse(ordCMPDS%in%cpdComb, bcNum, "black")
#postscript('figures/profilesgray.eps', width=11, height=7, colormodel=c("gray"), horizontal=FALSE)
	par(mar=c(6,4,2,2), xpd=TRUE)
#col = gray(seq(0.9,0,len =nrow(plotData)))
	col <-  rainbow(10)
	plot(plotData[1,],type='n', 
			ylim=c(min(plotData), max(plotData)), 
			xlim=c(1,ncol(eMat)),
			ylab = expression(log[2] ~ 
							paste("fold ", "change", " (centered)")),
			xlab=" ",
# \n \n (c) Profiles plot of the top differentially expressed genes for compounds tanespimycin and geldanamycin.",
			axes=FALSE,
			cex.lab=1
	)
	for (i in 1:nrow(plotData)){
		lines(plotData[i,], col=col[i], lwd=3)
	}
	legend("topright",inset(-2,0),
			legend=rownames(plotData), 
			text.col=col[1:nrow(plotData)],
			bty='n', cex=1 )
#axis(1, at=1:ncol(plotData), labels=substr(colnames(plotData),1,15), padj=1, las=2)
	axis(2)
	axis(1, labels=FALSE)
#box("outer")
	mtext(substr(colnames(plotData),1,15), side = 1,  at=1:ncol(plotData), line=1, las=2, cex=0.8,col=colorlead)
	mtext("Compounds", side = 1,  line=4, las=0, 
			cex=1)
#dev.off()

}


