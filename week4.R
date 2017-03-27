
rm(list = ls())
library(SnowballC) # for steming words.
library(ngram) # for n-grams model

unigram_dataframe<-readRDS(file = "./CapstoneProject/unigram_dataframe.rds")
bigram_dataframe<-readRDS(file = "./CapstoneProject/bigram_dataframe.rds")
trigram_dataframe<-readRDS(file = "./CapstoneProject/trigram_dataframe.rds")
quadrigram_dataframe<-readRDS(file = "./CapstoneProject/quadrigram_dataframe.rds")

CleaningText <- function(input_text)
{
  corpus <- Corpus(VectorSource(input_text))
  
  #removing the punctuation
  corpus <- tm_map(corpus, removePunctuation)
  #clearing the numbers
  corpus <- tm_map(corpus, removeNumbers)
  #transforming the corpus into lowercase
  corpus <- tm_map(corpus, tolower)
  #removing non-english words
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  #Stem words in a text document using Porter's stemming algorithm.
  corpus <- tm_map(corpus, stemDocument)
  #removing the blanks spaces
  corpus <- tm_map(corpus, stripWhitespace)
  # Plain text
  corpus  <- tm_map(corpus , PlainTextDocument)
  words <- strsplit(corpus$content$content, split = " ")[[1]]
  return(as.character(words))
}


Bigram_Prediction<-function(words)
{
  result <-c()
  allwords_bi <-c()
  allwords_uni <-c()
  probability <-c()
  for (i in 1:length(words))
  {
    term<-paste("^","\\b", words[i],"\\b", sep="")
    uni_term<-df_unigram[grep(term, df_unigram$terms),]
    bi_term<-df_bigram[grep(term, df_bigram$terms),]
    #final4Data$unigram==words[i]
    allwords_bi <-rbind(allwords_bi,bi_term)
    allwords_uni <-rbind(allwords_uni,uni_term)
    probability <-c(probability, allwords_bi$count/allwords_uni$count)
  }
  
  if(length(probability)>0){
    allwords<-matrix(unlist(strsplit(as.character(allwords_bi$terms[order(probability)]), " ")), ncol=2, byrow=TRUE)
    result <- allwords[dim(allwords)[1],2]
  }
  
  return(result)
  
}


Ngram_Prediction<-function(input_text, word_number)
{
  result <-c()
  words <- CleaningText(input_text)
  if(length(words)==0)
    return(result)
  if(word_number == 3)
  {
    if(length(words) >=3)
    {
      words <- words[(length(words)-2):length(words)]
    }
    df1 <-quadrigram_dataframe
    df2 <-trigram_dataframe
    W <- paste(words[1], words[2],words[3], sep=" ")
    term<- paste("^\\b",W,"\\b", sep="")

  }
  if(word_number == 2)
  {
    if(length(words) >=2)
    {
      words <- words[(length(words)-1):length(words)]
    }
    df1 <-trigram_dataframe
    df2 <-bigram_dataframe
    W <- paste(words[1], words[2], sep=" ")
    term<- paste("^\\b",W, "\\b", sep="")
  }
  if(word_number == 1)
  {
    if(length(words) >=1)
    {
      words <- words[length(words)]
    }
    df1 <-bigram_dataframe
    df2 <-unigram_dataframe
    W <- paste(words[1], sep=" ")
    term<- paste("^\\b",W, "\\b", sep="")
  }
  df1_term<-df1[grep(term, df1$Terms),]
  df2_term<-df2[grep(term, df2$Terms),]
  
  
  if(nrow(df1_term)>0){
    temp_prob <-c()
    for (i in 1: nrow(df1_term)) {
      temp_prob<-rbind(temp_prob, data.frame("terms"=df1_term[i,1], "prob"=df1_term[i,2]/df2_term[1,2]))
    }
    high_prob<-temp_prob[order(temp_prob$prob),]
    allwords<- matrix(unlist(strsplit(as.character(high_prob$terms), " ")), ncol= word_number + 1, byrow=TRUE)
    result <- allwords[dim(allwords)[1],word_number +1]
  } 
  
  return(result)
  
}






