path <- getwd()

file_list <-list.files( path = paste(path , .Platform$file.sep , "CapstoneProject", sep ="") ,pattern = "zip")
if(length(file_list)==0) 
{
  fileURL <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(fileURL, destfile = "Corpus.zip")
  unlink(fileURL)
  unzip("Corpus.zip")
  
}



  blogs_file <- paste(path, .Platform$file.sep,"CapstoneProject/final/en_US/en_US.blogs.txt" ,sep = "")#
  news_file <-paste(path, .Platform$file.sep,"CapstoneProject/final/en_US/en_US.news.txt", sep ="")
  twitter_file <-paste(path, .Platform$file.sep,"CapstoneProject/final/en_US/en_US.twitter.txt", sep ="")
  
  corpus.blogs <- readLines(blogs_file, encoding = "UTF-8")
  corpus.news<- readLines(news_file, encoding = "UTF-8")
  corpus.twitter <- readLines(twitter_file, encoding = "UTF-8")
  
  # Checking the size, number of lines and number of words for every corpus file
  #size
  corpus.blogs.size <- format(file.info(blogs_file)$size/1024.0^2, digits =2)
  corpus.news.size <- format(file.info(news_file)$size /1024.0^2, digits =2)
  corpus.twitter.size <- format(file.info(twitter_file)$size/1024.0^2, digits =2)
  
  #length
  corpus.blogs.length <- length(corpus.blogs)
  corpus.news.length <- length(corpus.news)
  corpus.twitter.length <- length(corpus.twitter)
  
  #number of words
  corpus.blogs.words_number <- length(unlist(strsplit(corpus.blogs, " "))) 
  corpus.news.words_number <- length(unlist(strsplit(corpus.news, " "))) 
  corpus.twitter.words_number <- length(unlist(strsplit(corpus.twitter, " "))) 
  
  #statistics output
  paste("BLOGS statistics: file size = ", corpus.blogs.size, "Mb, number of lines:", corpus.blogs.length, ", number of words:", corpus.blogs.words_number)
  paste("NEWS statistics: file size = ", corpus.news.size, "Mb, number of lines:", corpus.blogs.length, ", number of words:", corpus.news.words_number)
  paste("TWITTER statistics: file size = ", corpus.twitter.size, "Mb, number of lines:", corpus.twitter.length, ", number of words:", corpus.twitter.words_number)
  
  rm(blogs_file, news_file, twitter_file, corpus.blogs.size,corpus.news.size,corpus.twitter.size )
  rm(corpus.blogs.length,corpus.news.length,corpus.twitter.length )
  rm(corpus.blogs.words_number,corpus.news.words_number,corpus.twitter.words_number)
  
  
  #loading the library
  library(tm)
  library(SnowballC)
  #Sampling the corpus to 2000 documents
  set.seed(34568)
  corpus.blogs.sampled <- sample(corpus.blogs,1000)
  corpus.news.sampled <- sample(corpus.news,1000)
  corpus.twitter.sampled <- sample(corpus.twitter,1000)
  rm(corpus.blogs, corpus.news, corpus.twitter)
  
  #Building a  whole corpus 
  vector_corpus <- VectorSource(iconv(c(corpus.blogs.sampled, corpus.news.sampled, corpus.twitter.sampled), 'UTF-8', 'ASCII'))
  rm(corpus.blogs.sampled, corpus.news.sampled, corpus.twitter.sampled)
  
  working_corpus_sampled <- VCorpus(vector_corpus)
  rm(vector_corpus)
  
  #Cleaning the corpus
  #transforming the corpus into a a vector
  #working_corpus_sampled <- iconv(working_corpus_sampled, 'UTF-8', 'ASCII', "byte")

  #transforming the corpus into lowercase
  working_corpus_sampled <- tm_map(working_corpus_sampled, tolower)
  #removing non-english words
  working_corpus_sampled <- tm_map(working_corpus_sampled, removeWords, stopwords("english"))
  #removing the punctuation
  working_corpus_sampled <- tm_map(working_corpus_sampled, removePunctuation)
  #clearing the numbers
  working_corpus_sampled <- tm_map(working_corpus_sampled, removeNumbers)
  #Stem words in a text document using Porter's stemming algorithm.
  working_corpus_sampled <- tm_map(working_corpus_sampled, stemDocument)
  #removing the blanks spaces
  working_corpus_sampled <- tm_map(working_corpus_sampled, stripWhitespace)
  # Plain text
  working_corpus_sampled  <- tm_map(working_corpus_sampled , PlainTextDocument)
  
  #saving intermediate data
  saveRDS(working_corpus_sampled,file = "./CapstoneProject/working_corpus_sampled.rds")

  
  library(wordcloud)
  
  #Document term frequency
  dtm_corpus_sampled <- DocumentTermMatrix(working_corpus_sampled)
  dtm_corpus_sampled_mostfrequentterms <- removeSparseTerms(dtm_corpus_sampled, 0.999)
  dtm_corpus_sampled_mostfrequentterms$dimnames$Terms
  
  wordcloud(working_corpus_sampled, max.words = length(dtm_corpus_sampled$dimnames$Terms), random.order = FALSE,use.r.layout=FALSE)
  title("Whole corpus word cloud")
 
  