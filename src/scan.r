library(SnowballC)
library(tm)

GetTermVector <- function(file_dir){
  my.corpus <- Corpus(file_dir, readerControl = list(language = "lat"))
  my.corpus <- tm_map(my.corpus, function(x) gsub("[^[:alnum:]]", " ", x))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, tolower)
  # for some version need transformer : tm_map(my.corpus, content_transformer(tolower))
  my.corpus <- tm_map(my.corpus, stemDocument, language = "english")  
  my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus <- tm_map(my.corpus, PlainTextDocument)
  # for some version need reset : Corpus(VectorSource(my.corpus))
  
  return(data.frame(as.list(apply(TermDocumentMatrix(my.corpus), 1, sum))))
}

ShortVectoLong <- function(long_term, short_vec){
  if(length(short_vec)==0) return(rep(1,length=length(long_term)))
  
  loc <- which(long_term %in% colnames(short_vec))
  loc_word <- long_term[loc]
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == colnames(short_vec))] ) 
  }
  
  return(long_vec)
}

EM_Algorithm <- function(word_freq, tau, tdm){
  TPWM <- as.data.frame(matrix(NA, nrow = nrow(tdm), ncol = ncol(tdm)))
  for(i in 1:8){
    denom <- apply(tdm,1, function(x)sum(tau*x))
    for(j in 1:ncol(tdm)){ 
      TPWM[,j] <- tau[j]*tdm[,j] / denom
    }
    
    tau <- apply(TPWM, 2, function(x)sum(word_freq*x) / sum(word_freq))
    
    # log_like <- sum(mapply( function(x,y) y*log(sum(tau*x)), 
    #                         as.data.frame(t(tdm)), 
    #                         as.list(word_freq)))
    # print(log_like)
  }
  
  catego[ which(tau == max(tau)) ]
}

##### main #####

### set all file path
setwd("~/Desktop/text-mining-in-EM-algorithm/test")
catego <- list.dirs('Train', recursive=FALSE)

### generate term freq. in each topic
term_vec <- list()
for(i in 1: length(catego)){
  term_vec[[i]] <- GetTermVector(DirSource(catego[i]))
}

### All term for EM analysis
all_term <- colnames(term_vec[[1]])
for(i in 2:length(catego)){
  all_term <- union(all_term, colnames(term_vec[[i]]))
}

### generate term document matrix
tdm <- as.data.frame(matrix(NA, nrow = length(all_term), ncol = length(catego)))
for(i in 1:length(catego)){
  long_vec <- ShortVectoLong(all_term, term_vec[[i]])
  tdm[,i] <- long_vec / sum(long_vec)
}
# View(tdm)

### handle query document 
all_test <- DirSource('Test')
all_test <- lapply(all_test$filelist, function(x)substring(x,6,9))
all_test <- paste0(rep('Test/',length(all_test)), 
                   as.character(sort(as.numeric(all_test))))
all_test <- all_test

### EM Algorithm
ans_list <- vector(mode="character",length=length(all_test))
names(ans_list) <- 1:length(all_test)

for(i in 1:length(all_test)){
  
  flag_time = Sys.time()
  
  query_term <- GetTermVector(VectorSource(readLines(all_test[i])))
  ans_list[i] <- EM_Algorithm(word_freq = ShortVectoLong(all_term, query_term),
                              tau = rep(1/length(catego), length(catego)),
                              tdm = tdm
                             )
  print( paste(i, ans_list[i], collapse = " "))
  
  print(Sys.time() - flag_time)
}




# test
query_term <- GetTermVector(VectorSource(readLines(all_test[353])))
word_freq = ShortVectoLong(all_term, query_term)
tau = rep(1/length(catego), length(catego))
tdm = tdm






