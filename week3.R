rm(list = ls())

#loading the libraries
#Creating the ngrams and saving them into files
library(RWeka)
#Data mining
library(tm)
library(SnowballC)

working_corpus_sampled <-readRDS(file = "./CapstoneProject/working_corpus_sampled.rds")

#Spliting strings into n-grams with given minimal and maximal numbers of grams.
#Applied for 1,2,3 and 4 grams
options(mc.cores=1)
memory.limit(4095)
unigram <- NGramTokenizer(working_corpus_sampled, Weka_control(min = 1, max = 1)) 
#saving intermediate data
saveRDS(unigram,file = "./CapstoneProject/unigram.rds")
rm(unigram)
bigram <- NGramTokenizer(working_corpus_sampled, Weka_control(min = 2, max = 2))
#saving intermediate data
saveRDS(bigram,file = "./CapstoneProject/bigram.rds")
rm(bigram)
trigram <- NGramTokenizer(working_corpus_sampled, Weka_control(min = 3, max = 3))
#saving intermediate data
saveRDS(trigram,file = "./CapstoneProject/trigram.rds")
rm(trigram)
quadrigram <- NGramTokenizer(working_corpus_sampled, Weka_control(min = 4, max = 4))
#saving intermediate data
saveRDS(quadrigram,file = "./CapstoneProject/quadrigram.rds")

#Dataframes for the 1,2,3 and 4 grams

##unigram
unigram <- readRDS("./CapstoneProject/unigram.rds")
#Tokenizing functions
unigram_tokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
#Converting the n-grams into a term document matrix
unigram_tdm<-TermDocumentMatrix(working_corpus_sampled, control = list(tokenize = unigram_tokenizer))
#unigram_tdm <- removeSparseTerms(unigram_tdm, 0.9998)
#Dataframe
unigram_dataframe<-as.data.frame(as.matrix(unigram_tdm))
#counting
unigram_dataframe$Count<-rowSums(unigram_dataframe)
#subsetting those with more than 1 appearance
#unigram_dataframe<-subset(unigram_dataframe, Count> 1)
#assigning the name of the terms to the 
unigram_dataframe$Terms<-row.names(unigram_dataframe)
#Sorting ascending
unigram_dataframe<-unigram_dataframe[order(-unigram_dataframe$Count),]
row.names(unigram_dataframe)<-NULL
unigram_dataframe<-subset(unigram_dataframe, select=c("Terms","Count"))
saveRDS(unigram_dataframe,file = "./CapstoneProject/unigram_dataframe.rds")
rm(unigram_dataframe, unigram_tdm, unigram, unigram_tokenizer)

##bigram
bigram <- readRDS("./CapstoneProject/bigram.rds")
#Tokenizing functions
bigram_tokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#Converting the n-grams into a term document matrix
bigram_tdm<-TermDocumentMatrix(working_corpus_sampled, control = list(tokenize = bigram_tokenizer))
#bigram_tdm_s <- bigram_tdm[bigram_tdm$v >1]
#Dataframe
bigram_dataframe<-as.data.frame(as.matrix(bigram_tdm))
#counting
bigram_dataframe$Count<-rowSums(bigram_dataframe)
#subsetting those with more than 1 appearance
#bigram_dataframe<-subset(bigram_dataframe, Count> 1)
#assigning the name of the terms to the 
bigram_dataframe$Terms<-row.names(bigram_dataframe)
#Sorting ascending
bigram_dataframe<-bigram_dataframe[order(-bigram_dataframe$Count),]
row.names(bigram_dataframe)<-NULL
bigram_dataframe<-subset(bigram_dataframe, select=c("Terms","Count"))
saveRDS(bigram_dataframe,file = "./CapstoneProject/bigram_dataframe.rds")
rm(bigram_dataframe, bigram_tdm, bigram, bigram_tokenizer)

##Trigram
trigram <- readRDS("./CapstoneProject/trigram.rds")
#Tokenizing functions
trigram_tokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
#Converting the n-grams into a term document matrix
trigram_tdm<-TermDocumentMatrix(working_corpus_sampled, control = list(tokenize = trigram_tokenizer))
#trigram_tdm_s <- removeSparseTerms(trigram_tdm, 0.999887)
#Dataframe
trigram_dataframe<-as.data.frame(as.matrix(trigram_tdm))
#counting
trigram_dataframe$Count<-rowSums(trigram_dataframe)
#subsetting those with more than 1 appearance
#trigram_dataframe<-subset(trigram_dataframe, Count> 1)
#assigning the name of the terms to the 
trigram_dataframe$Terms<-row.names(trigram_dataframe)
#Sorting ascending
trigram_dataframe<-trigram_dataframe[order(-trigram_dataframe$Count),]
row.names(trigram_dataframe)<-NULL
trigram_dataframe<-subset(trigram_dataframe, select=c("Terms","Count"))
saveRDS(trigram_dataframe,file = "./CapstoneProject/trigram_dataframe.rds")
rm(trigram_dataframe, trigram_tdm, trigram, trigram_tokenizer)


##Quadrigram
quadrigram <- readRDS("./CapstoneProject/quadrigram.rds")
#Tokenizing functions
quadrigram_tokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
quadrigram_tdm<-TermDocumentMatrix(working_corpus_sampled, control = list(tokenize = quadrigram_tokenizer))
#Dataframe
quadrigram_dataframe<-as.data.frame(as.matrix(quadrigram_tdm))
#counting
quadrigram_dataframe$Count<-rowSums(quadrigram_dataframe)
#subsetting those with more than 1 appearance
#quadrigram_dataframe<-subset(quadrigram_dataframe, Count> 1)
#assigning the name of the terms to the 
quadrigram_dataframe$Terms<-row.names(quadrigram_dataframe)
#Sorting ascending
quadrigram_dataframe<-quadrigram_dataframe[order(-quadrigram_dataframe$Count),]
row.names(quadrigram_dataframe)<-NULL
quadrigram_dataframe<-subset(quadrigram_dataframe, select=c("Terms","Count"))
#saving data
saveRDS(quadrigram_dataframe,file = "./CapstoneProject/quadrigram_dataframe.rds")

rm(quadrigram_dataframe, quadrigram_tdm, quadrigram, quadrigram_tokenizer)
rm(working_corpus_sampled)