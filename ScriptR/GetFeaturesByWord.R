if(!require("text2vec"))
  install.packages("text2vec", dependencies=TRUE)
library(text2vec)

if(!require("devtools"))
  install.packages("devtools", dependencies=TRUE)
library(devtools)


if(!require("keras")){
  devtools::install_github("rstudio/keras")
  keras::install_keras(tensorflow="1.2.1-gpu") # instead of last version you can use a previous one like "1.2.1-gpu"
}
library(keras)

if(!require("iterators"))
  install.packages("iterators", dependencies=TRUE)
library(iterators)


if(!require("yaml"))
  install.packages("yaml", dependencies=TRUE)
library(yaml)


if(!require("reticulate"))
  install.packages("reticulate", dependencies=TRUE)
library(reticulate)


if(!require("foreach"))
  install.packages("foreach", dependencies=TRUE)
library(foreach)


library(tm)

load("../embedding.RData")

similar.words <- function(v, n=30,min=0.0) {
  result = head(sort(sim2(wordvecs, matrix(v, 1, ncol(wordvecs)), method="cosine", norm="l2")[,1], decreasing=T), n)
  return(result[result<0.9999 & result>min])
}

featuresByWord <- function(word){
  names(similar.words(wordvecs[word,],min=0.7))
}
