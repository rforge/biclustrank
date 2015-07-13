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
plotFabia<- function(resFabia,bicRes, bcNum, plot=c(1,2)){
	if(plot==1){
		par(mar=c(2,3.5,2,2))
		plot(resFabia@L[,bcNum],  type="p",
#		main=paste("FABIA factor loadings",bcNum, sep=""),
#		xlab="gene index", 
				ylab=" ",
				pch=21,
				bg="grey",
				col="grey",
				cex=2,
#		ylim = c(-0.4, 0.1),
				axes=FALSE
		)
		gComb <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 2) )
		labelGpos <- match(gComb,rownames(resFabia@L))
		text(labelGpos,resFabia@L[gComb,bcNum], gComb,
				pos=1,
				cex=0.8,
				col=bcNum)
		points(labelGpos, resFabia@L[gComb,bcNum], 
				
				pch=21,  bg="grey", cex=2, col="black"
		)
#points(labelgClust, fabRes@L[labelgClust,bcNum], 
#
#		pch=21, bg="grey"
#)
		axis(2)
		mtext(" Loadings", side = 2,  line=2, las=0, 
				cex=1.5)
		mtext("Gene index", side = 1,  line=0.5, las=0, 
				cex=1.5)
#legend("topright",c("Fabia","HC"),
#		text.col = c("blue","black"), pch=c(24,21), pt.bg=c("grey",'grey'))
		box()
	}
	else if(plot==2){
		
		par(mar=c(2,3.5,2,2))
		plot(resFabia@Z[bcNum,], 
				ylab=" ",
				pch=21,
				cex=2,
				col="grey",bg="grey",
				axes=FALSE)
		cFabia <- unlist(sapply(bicRes[paste("BC",bcNum,sep="")], "[[", 1) )
#cFabia <- bicList[[bcNum]][[1]]
		labelCpos <- match(cFabia,names(resFabia@Z[bcNum,]))
		points(labelCpos, as.vector(resFabia@Z[bcNum,cFabia]), 
				pch=21, bg="grey", col=bcNum, cex=2
		)
		text(labelCpos, as.vector(resFabia@Z[bcNum,cFabia]), 
				cFabia,
				cex=0.8,
				col=bcNum, pos=3
		)
		
		axis(2)
		mtext("FABIA Scores", side = 2,  line=2, las=0, 
				cex=1.5)
		mtext("Compound index", side = 1,  line=0.5, las=0, 
				cex=1.5)
		box()
		
	}
	else{stop("plot should be either 1=loadings or 2=scores but not both.")
	}
	
}