library(tidyverse)
library(plyr)
library(dplyr)
require(stringr)
#Ottenimento degli opinion lexicon per la lingua italiana
pos.words = scan('../OpinionLexicon/positive_words_it.txt', what='character', comment.char=';')
neg.words = scan('../OpinionLexicon/negative_words_it.txt', what='character', comment.char=';')


getReviews = function(FILE){
  read.csv(FILE,stringsAsFactors = FALSE, sep=",",fill=TRUE)$Restaurant_Reviews[-1]
}

scoreByFeatureAndReviews = function(FEATURE,REVIEWS){
  data.text <- sapply(REVIEWS, function(x) gsub("\\!+",".",x))
  data.text <- sapply(data.text, function(x) gsub("\\?+",".",x))
  data.text <- sapply(data.text, function(x) gsub("\\,+",".",x))
  data.text <- sapply(data.text, function(x) strsplit(x,"\\."), simplify=F)
  data.sentences <- ""
  for(s in data.text){
    s.list=unlist(s)
    data.sentences<-c(data.sentences,s.list)
  }
  data.score.feature <- score.featuresentiment(data.sentences,FEATURE,pos.words,neg.words)
}



score.featuresentiment = function(sentences, feature, pos.words,
                                  neg.words, .progress='none'){
  scores = laply(sentences, function(sentence, feature, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    feat.matches = match(words, feature)
    score=0 
    for(m in feat.matches){
      if(!is.na(m)){
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        score = sum(pos.matches) - sum(neg.matches)
      }
    }
    return(score)
  }, feature, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

computeAndAggregateScores = function(FILES,NAMES,FEATURE){
  firstRestaurant<-getReviews(FILES[1])
  firstRestaurant.score.element <- scoreByFeatureAndReviews(FEATURE,firstRestaurant)
  firstRestaurant.score.element$name = sub('\\..*', '', NAMES[1])
  data.all.scores = rbind(firstRestaurant.score.element)
  numR<-length(FILES)
  for(i in 1:numR){
    restaurant <- getReviews(FILES[i])
    restaurant.score.element <- scoreByFeatureAndReviews(FEATURE,restaurant)
    restaurant.score.element$name = sub('\\..*', '', NAMES[i])
    data.all.scores = rbind(data.all.scores,restaurant.score.element)
  }
  data.all.scores
}

getRankingByFeature = function(FEATURE,FILES,NAMES,MIN){
  
  data.all.scores<-computeAndAggregateScores(FILES,NAMES,FEATURE)
  
  
  data.all.scores$pos = as.numeric( data.all.scores$score >0 )
  data.all.scores$neg = as.numeric( data.all.scores$score <0 )
  
  data.scores.df = ddply(data.all.scores, c('name'),
                         summarise,pos.count = sum( pos ), neg.count = sum( neg ) )
  data.scores.df$all.count = data.scores.df$pos.count + data.scores.df$neg.count

  data.scores.df$score = round( 50 * data.scores.df$pos.count / data.scores.df$all.count)
  data.scores.df <- subset(data.scores.df,all.count >MIN)
  ordered<-arrange(data.scores.df, -score)
}
source("GetFeaturesByWord.r")
word = "piadina"
feature <-featuresByWord(word)

reviewPath<-"../RecensioniCesena"
names <- list.files(path=reviewPath,pattern = "\\.csv$")
names <- names[names != "allReviews.csv"]
files=file.path(reviewPath,names)

minCount<-30
ranking<-getRankingByFeature(feature,files,names,minCount)
ranking

ggplot(ranking,aes(x=word,y=score,fill=name))+geom_bar(stat="identity",position="dodge")





