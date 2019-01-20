library(tm)
library(lsa)
library(NLP)
library(RWeka)

getReviews = function(FILE){
  read.csv(FILE,stringsAsFactors = FALSE, sep=",",fill=TRUE)$Restaurant_Reviews[-1]
}


prepareCorpus = function(FILENAME){
  allReviews <- getReviews(FILENAME)
  data<-VectorSource(allReviews)
  corpus <- Corpus(data)
  stoplist<-stopwords("it") # List standard Italian stop words
  corpus <- tm_map( corpus, tolower )
  corpus <- tm_map( corpus, removePunctuation )
  corpus <- tm_map( corpus, removeWords, stoplist )
}

createReducedTDM = function(CORPUS,SPARSE){
  tdmc <- TermDocumentMatrix( CORPUS )
  tdms <- removeSparseTerms(tdmc,SPARSE)
}

createTableName = function(TABLENAME){
  paste(sub('\\..*', '_', TABLENAME),"tdm.table",sep="")
}

corpus<-prepareCorpus(fileName)
tdms <- createReducedTDM(corpus,0.99)
save(tdms,file=createTableName(tableName))
