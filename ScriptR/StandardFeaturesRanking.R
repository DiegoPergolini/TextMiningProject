library(plyr)
library(dplyr)
require(stringr)
library(tidyverse)
library(reshape2)
#Ottenimento degli opinion lexicon per la lingua italiana
pos.words = scan('../OpinionLexicon/positive_words_it.txt', what='character', comment.char=';')
neg.words = scan('../OpinionLexicon/negative_words_it.txt', what='character', comment.char=';')

feature.atmosfera <- scan('../Features/feature-atmosfera.txt',what="character")[-1]
feature.servizio <- scan('../Features/feature-servizio.txt',what="character")[-1]
feature.qualitaPrezzo <- scan('../Features/feature-qualitaPrezzo.txt',what="character")[-1]
feature.cucina <- scan('../Features/feature-cucina.txt',what="character")[-1]


feature.servizio<-unique(c(feature.servizio,'servizio','amichevole','disponibile','cordialita','professionalita',
                     'cameriere','proprietario','colazione','pranzo','cena','portata','bar',
                     'camerieri','velocita','posate','piatti','pulizia','maleducati','distaccati'))
feature.qualitaPrezzo <- unique(c(feature.qualitaPrezzo,'qualita','prezzo','valore','costo','soldi','euro','oneroso','costoso',
                          'risparmio','economico','lusso'))
feature.atmosfera <-unique(c(feature.atmosfera,'atmosfera','rumore','silenzio','divertimento','svago','bambini',
                      'felicita', 'gioia', 'lugubre','gentilezza','locale','ambiente',
                      'gentili','sorridenti','chiacchere','scortesia','scortesi',
                      'antipatici','maleducati','distaccati'))



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
  data.scores.df
}
reviewPath<-"../RecensioniCesena"
cityName <-"Cesena"
names <- list.files(path=reviewPath,pattern = "\\.csv$")
files=file.path(reviewPath,names)

minCount<-100
ranking.cucina<-getRankingByFeature(feature.cucina,files,names,minCount)
ranking.servizio<-getRankingByFeature(feature.servizio,files,names,minCount)
ranking.qualitaPrezzo<-getRankingByFeature(feature.qualitaPrezzo,files,names,minCount)
ranking.atmosfera<-getRankingByFeature(feature.atmosfera,files,names,minCount)
ranking.cucina
ranking.servizio
ranking.qualitaPrezzo
ranking.atmosfera


ranking.aggregated.cucina <- select(ranking.cucina,name,score)
names(ranking.aggregated.cucina )[2]<-"Cucina"

ranking.aggregated.servizio <- select(ranking.servizio,name,score)
names(ranking.aggregated.servizio )[2]<-"Servizio"

ranking.aggregated.qualitaPrezzo <- select(ranking.qualitaPrezzo,name,score)
names(ranking.aggregated.qualitaPrezzo )[2]<-"QualitaPrezzo"

ranking.aggregated.atmosfera <- select(ranking.atmosfera,name,score)
names(ranking.aggregated.atmosfera )[2]<-"Atmosfera"

total <- merge(ranking.aggregated.cucina,
               ranking.aggregated.servizio,by="name")
total<- merge(total,ranking.aggregated.qualitaPrezzo,by="name")
total<- merge(total,ranking.aggregated.atmosfera,by="name")
total

totalRankingMean <-data.frame(name=total[,1], Means=rowMeans(total[,-1]),stringsAsFactors = FALSE)
totalRankingMean <- subset(totalRankingMean,!is.na(Means ))

ordered<-arrange(totalRankingMean, -Means)
ordered

completeRanking<-merge(total,totalRankingMean,by="name")
completeRanking<-arrange(completeRanking, -Means)
top10<-completeRanking[1:15,]


top10<-top10[,2:length(top10)-1]

top10.long<-melt(top10,id.vars="name")
ggplot(top10.long,aes(x=variable,y=value,fill=name))+geom_bar(stat="identity",position="dodge")
fileName<-paste("CompleteRanking",cityName,".csv",sep = "")
write.table(completeRanking,file=fileName,sep=",",fileEncoding = "UTF-8")

       