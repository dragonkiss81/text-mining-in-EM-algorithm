library(SnowballC)
library(tm)

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
         
tdm <- list()

for(i in 1: length(txt)){
  my.corpus <- Corpus(DirSource(txt[i]), readerControl = list(language = "lat"))
  my.corpus <- tm_map(my.corpus, function(x) gsub("[^[:alnum:]]", " ", x))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, tolower)
  # my.corpus <- tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, stemDocument, language = "english")  
  my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus <- Corpus(VectorSource(my.corpus))
  
  df <- data.frame(as.list(apply(TermDocumentMatrix(my.corpus), 1, sum)))
  df
  tdm[[i]] <- df
}

# All term for EM analysis
all_term <- colnames(tdm[[1]])
for(j in 2:length(txt)){
  all_term <- union(all_term, colnames(tdm[[j]]))
}

