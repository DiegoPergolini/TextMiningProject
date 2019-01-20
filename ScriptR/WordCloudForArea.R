library(tm)
library(lsa)
library(NLP)
library(RWeka)
library(wordcloud)
getReviews = function(FILE){
  read.csv(FILE,stringsAsFactors = FALSE, sep=",",fill=TRUE)$Restaurant_Reviews[-1]
}
allReviews <- getReviews("../RecensioniMixate/allReviewsC.csv")


data<-VectorSource(allReviews)
corpus <- Corpus(data)
stoplist<-stopwords("it") # List standard Italia stop words
corpus <- tm_map( corpus, tolower )
corpus <- tm_map( corpus, removePunctuation )
corpus <- tm_map( corpus, removeWords, stoplist )

review_dtm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.99)
freq = data.frame(sort(rowSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
dev.new(width = 1024, height = 1024, unit = "px",noRStudioGD = TRUE)

wordcloud(rownames(freq), freq[,1], scale=c(4,.1),random.order=FALSE,  max.words=100, colors=brewer.pal(8, "Dark2"))
