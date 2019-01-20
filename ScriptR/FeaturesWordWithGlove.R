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

tokenize <- function(fulltext) word_tokenizer(tolower(fulltext))

getReviews = function(FILE){
  read.csv(FILE,stringsAsFactors = FALSE, sep=",",fill=TRUE)$Restaurant_Reviews[-1]
}

loading_time_start = proc.time()
we.train.set <- getReviews("./allReviewsCJFN.csv")
loading_time_end = proc.time()
cat("loading time:", loading_time_end[[3]] - loading_time_start[[3]])

length(we.train.set)
head(we.train.set)  
stoplist <- stopwords('it')



we.train.tokens <- tokenize(we.train.set)


we.train.itoken <- itoken(we.train.tokens)


vocab <- create_vocabulary(we.train.itoken,stopwords = stoplist)

length(vocab$term)

vocab <- prune_vocabulary(vocab, term_count_min=5)

vocab.size <- length(vocab$term)
vocab.size


we.vect <- vocab_vectorizer(vocab)
View(vocab)

we.train.tcm <- create_tcm(we.train.itoken, we.vect, skip_grams_window=10)

wordvecs.size <- 50
glove.model <- GloVe$new(word_vectors_size=wordvecs.size, vocabulary=vocab, x_max=10)
wordvecs <- glove.model$fit_transform(we.train.tcm, n_iter=10)

save(wordvecs, file="embedding.RData")

wordvecs["piadina",]



similar.words <- function(v, n=30,min=0.0) {
  result = head(sort(sim2(wordvecs, matrix(v, 1, ncol(wordvecs)), method="cosine", norm="l2")[,1], decreasing=T), n)
  return(result[result<0.9999 & result>min])
}


numFeature <-100
feature.ambiente<-similar.words(wordvecs["ambiente",],numFeature,min=0.6)
feature.ambiente.num <- length(feature.ambiente)
feature.ambiente.words<-names(feature.ambiente[1:feature.ambiente.num])
numFeature <-100
feature.atmosfera<-similar.words(wordvecs["atmosfera",],numFeature,min=0.6)
feature.atmosfera.num <- length(feature.atmosfera)
feature.atmosfera.words<-names(feature.atmosfera[1:feature.atmosfera.num])

feature.atmosfera.words<-c(feature.atmosfera.words,feature.ambiente.words)
feature.atmosfera.words <- unique(feature.atmosfera.words)

write.table(feature.atmosfera.words, file = "feature-atmosfera.txt", sep = "\n",
            row.names = FALSE)


numFeature <-200
feature.cucina<-similar.words(wordvecs["cucina",],numFeature,min=0.604)
feature.cucina.num <- length(feature.cucina)
feature.cucina.words<-names(feature.cucina[1:feature.cucina.num])

write.table(feature.cucina.words, file = "feature-cucina.txt", sep = "\n",
            row.names = FALSE)


feature.servizio<-similar.words(wordvecs["servizio",],numFeature,min=0.604)
feature.servizio.num <- length(feature.servizio)
feature.servizio.words<-names(feature.servizio[1:feature.servizio.num])

write.table(feature.servizio.words, file = "feature-servizio.txt", sep = "\n",
            row.names = FALSE)




feature.qualita<-similar.words(wordvecs["qualita",],100,min=0.604)
feature.qualita.num <- length(feature.qualita)
feature.qualita.words<-names(feature.qualita[1:feature.qualita.num])

write.table(feature.qualita.words, file = "feature-qualita.txt", sep = "\n",
            row.names = FALSE)

feature.prezzo<-similar.words(wordvecs["prezzo",],100,min=0.604)
feature.prezzo.num <- length(feature.prezzo)
feature.prezzo.words<-names(feature.prezzo[1:feature.prezzo.num])

write.table(feature.prezzo.words, file = "feature-prezzo.txt", sep = "\n",
            row.names = FALSE)

feature.qualitaprezzo <- c(feature.prezzo,feature.qualita)
feature.qualitaprezzo.num <- length(feature.qualitaprezzo)
feature.qualitaprezzo.words<-names(feature.qualitaprezzo[1:feature.qualitaprezzo.num])
feature.qualitaprezzo.words<-unique(feature.qualitaprezzo.words)
write.table(feature.qualitaprezzo.words, file = "feature-qualita-prezzo.txt", sep = "\n",
            row.names = FALSE)


similar.words(wordvecs["cameriere",] - wordvecs["uomo",] + wordvecs["donna",])
similar.words(wordvecs["pizze",] - wordvecs["pizza",] + wordvecs["piadina",])
similar.words(wordvecs["pizzeria",] - wordvecs["pizza",] + wordvecs["piadina",])
