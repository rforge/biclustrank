###############################################################################
# Project: biclustRank
## Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

###############################################################################

#' @title get similarity matrix of another data source
#' @param dataB = another data source not used in biclustering (similarity of rows)
#' @param distmeasure = distance measure
#' @return a matrix of distance coefficient
#' @author Nolen Joy Perualila (nolenjoy.perualila@uhasselt.be)

#Step 1: compute distance matrices:
Distance=function(dataB,distmeasure="tanimoto"){
	data <- dataB+0
	
	tanimoto = function(m){
		S = matrix(0,nrow=dim(m)[1],ncol=dim(m)[1])
		
		for(i in 1:dim(m)[1]){
			for(j in 1:i){
				N.A = sum(m[i,])
				N.B = sum(m[j,])
				N.C = sum(m[i,(m[i,]==m[j,])])
				
				coef = N.C / (N.A+N.B-N.C)
				S[i,j] = coef
				S[j,i] = coef
			}
			
		}
		D = 1 - S
		return(D)
	}
	
	# Computing the distance matrices
	
	if(distmeasure=="jaccard"){
		dist = dist.binary(data,method=1)
	
	}
	else if(distmeasure=="tanimoto"){
		dist = tanimoto(data)
	
	}
	else if(distmeasure=="euclidean"){
		dist = daisy(data,metric="euclidean")
			}
	
	else{
		stop("Incorrect choice of distmeasure. Must be one of: tanimoto, jaccard or euclidean.")
	}
	
	dist = as.matrix(dist)
	rownames(dist) <- colnames(dist) <- rownames(data)
	return(dist)
}

