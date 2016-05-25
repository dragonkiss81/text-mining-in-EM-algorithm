library(SnowballC)
library(tm)

### set all file path
setwd("~/Desktop/text-mining-in-EM-algorithm/test")
txt <- c("Train/alt.atheism", 
         "Train/comp.graphics", 
         "Train/comp.os.ms-windows.misc" , 
         "Train/comp.sys.mac.hardware" , 
         "Train/comp.windows.x" , 
         "Train/misc.forsale" , 
         "Train/rec.autos" , 
         "Train/rec.motorcycles" , 
         "Train/rec.sport.baseball" , 
         "Train/rec.sport.hockey" , 
         "Train/sci.crypt" , 
         "Train/sci.electronics" , 
         "Train/sci.med" , 
         "Train/sci.space" , 
         "Train/soc.religion.christian" , 
         "Train/talk.politics.guns" , 
         "Train/talk.politics.mideast" , 
         "Train/talk.politics.misc" , 
         "Train/talk.religion.misc"
         )

### generate term freq. in each topic
term_vec <- list()
for(i in 1: length(txt)){
  my.corpus <- Corpus(DirSource(txt[i]), readerControl = list(language = "lat"))
  my.corpus <- tm_map(my.corpus, function(x) gsub("[^[:alnum:]]", " ", x))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, tolower)
  # for some version : my.corpus <- tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, stemDocument, language = "english")  
  my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus <- tm_map(my.corpus, PlainTextDocument)
  # for some version need reset : my.corpus <- Corpus(VectorSource(my.corpus))
  
  df <- data.frame(as.list(apply(TermDocumentMatrix(my.corpus), 1, sum)))
  df
  term_vec[[i]] <- df
}

### All term for EM analysis
all_term <- colnames(term_vec[[1]])
for(j in 2:length(txt)){
  all_term <- union(all_term, colnames(term_vec[[j]]))
}

### corpus vector in each topic to all term vector
ShortVectoLong <- function(long_term, short_vec){
  loc <- which(long_term %in% colnames(short_vec))
  loc_word <- long_term[loc]
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == colnames(short_vec))] ) 
  }
  
  long_vec
}

### generate document term matrix
dtm <- as.data.frame(matrix(NA, nrow = length(txt), ncol = length(all_term)))
for(k in 1:length(txt)){
  dtm[k,] <- ShortVectoLong(all_term, term_vec[[k]])
}
View(dtm)




