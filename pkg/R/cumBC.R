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

cumBC <-
		function(LstTot,refScore=0.5,prob=TRUE,colour,...)
{
	
#	par(mfrow=c(1,1))
	l = length(LstTot)
	x <- lapply(LstTot,sort )
fun.ecdf <- sapply(1:l, function(row) ecdf(LstTot[[row]]))
y <- sapply(1:l,function(row) fun.ecdf[[row]](x[[row]]))
s <- sapply(1:l, function(row) 1-fun.ecdf[[row]](refScore))
names(s) <- names(LstTot)
sSort <- sort(s, decreasing=TRUE)
out <- mapply(cbind, x, y, SIMPLIFY=FALSE)

for(i in 1:l)
if(prob){
	
plot(x[[1]],y[[1]], type="n", main="Empirical cumulative distribution of \n chemical struture similarity scores", xlab="similarity score",
		ylab="cumulative probabilty", xlim=c(0,1), ylim=c(0,1),...)
colour = c(1:8,"salmon","red4")
for (i in 1:l)
{
	lines(x[[i]], y[[i]], type="s",col= colour[i])
}
}
abline(v=c(refScore),lty="dashed", col = 6)
legend(0.8,0.8,legend = names(LstTot), lty=c(rep(1,length(names(LstTot)))),
		pch =c(rep(19,length(names(LstTot)))),col=colour)

return(sSort)

}


