library(tm)
library(lsa)
library(NLP)
library(wordcloud)
restaurant<-"1947pizzafritta.csv"
reviewPath<-"../RecensioniNapoli/"
filename<-paste(reviewPath,restaurant,sep = "")
reviewsCS <- read.csv(filename,stringsAsFactors = FALSE,sep=",",fill=TRUE)
ratings <- reviewsCS$Restaurant_Ratings
ratings<-ratings[-1]
data <-reviewsCS$Restaurant_Reviews
data<-data[-1]
data<-VectorSource(data)
corpus <- Corpus(data)
stoplist<-stopwords("it")
stoplist<-c(stoplist,'uibubblerating')
corpus <- tm_map( corpus, tolower )
corpus <- tm_map( corpus, removePunctuation )
corpus <- tm_map( corpus, removeWords, stoplist )

review_dtm_tfidf <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf <- removeSparseTerms(review_dtm_tfidf, 0.99)
freq = data.frame(sort(rowSums(as.matrix(review_dtm_tfidf)), decreasing=TRUE))
dev.new(width = 1024, height = 1024, unit = "px",noRStudioGD = TRUE)

wordcloud(rownames(freq), freq[,1], scale=c(4,.1),random.order=FALSE,  max.words=100, colors=brewer.pal(8, "Dark2"))
