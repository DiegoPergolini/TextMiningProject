library(tm)
library(lsa)
library(NLP)
library(RWeka)


createTableName = function(FILENAME){
  paste(sub('\\..*', '_', fileName),"tdm.table",sep="")
}


getRatings = function(FILENAME){
  reviewsDataset <- read.csv(FILENAME,stringsAsFactors = FALSE,sep=",",fill=TRUE)
  ratings <- reviewsCS$Restaurant_Ratings
  ratings<-ratings[-1]
}
getReviews = function(FILE){
  read.csv(FILE,stringsAsFactors = FALSE, sep=",",fill=TRUE)$Restaurant_Reviews[-1]
}

getTdmTable = function(FILENAME){
  load(createTableName(FILENAME))
  as.matrix(tdms)
}

getDimensionToKeep = function(LSAR){
  fordiff <- function(x) x[2:length(x)] - x[1:(length(x)-1)]
  skd <- fordiff(LSAR$sk)
  skdd <- fordiff(skd)
  skcurv <- skdd[1:20] / (1+(skd[1:20])^2)^1.5
  
  min<-0
  dimensionToKeep <- 1
  for(i in 2:20){
    difference<-skcurv[i]-skcurv[i-1]
    if(difference < min){
      min <- difference
      dimensionToKeep<-i
    }
  }
  dimensionToKeep
}

makequery <- function(Q, TDM, LSA) {
  V <- query( Q, rownames(TDM) )
  W <- lw_logtf(V) * (1-entropy(TDM))
  LS <- t(W) %*% LSA$tk
  DK <- LS %*% diag(LSA$sk^-1)
  DKSRS <- DK %*% diag(sqrt(LSA$sk))
  list( query=Q, bin=V, v=W, dk=DK, ls=LS, dksrs=DKSRS )
}

getSimilarReviewsWithLSA = function(FILENAME,RESTAURANT,QUERY,NUMREVIEWS){
  reviews <- getReviews(FILENAME)
  tdm <- getTdmTable(RESTAURANT)
  
  words <- rownames(tdm)
  
  tdmle <- lw_logtf(tdm) * ( 1-entropy(tdm) )  
  
  
  lsar <- lsa(tdmle)
  
  numAutoV=length(lsar$sk)
  
  dimensionToKeep <- getDimensionToKeep(lsar)
  
  tls <- lsar$tk %*% diag( lsar$sk )
  dls <- lsar$dk %*% diag( lsar$sk )
  
  cosines <- function(X, Q) apply( X, 1, cosine, as.vector(Q) )
  top <- function(X, N) order( X, decreasing=TRUE )[1:N]
  
  awi <- makequery( QUERY, tdm, lsar )
  result<-reviews[top(cosines(dls[,1:dimensionToKeep], awi$ls[1:dimensionToKeep]), NUMREVIEWS)]
  result
}
restaurant<-"SfizidiPuglia.csv"
reviewPath<-"../RecensioniCesena/"
fileName<-paste(reviewPath,restaurant,sep = "")
tableName<-restaurant
source("CreateTdmTable.R")
query="panzerotti"
numResults=15
getSimilarReviewsWithLSA(fileName,restaurant,query,numResults)



