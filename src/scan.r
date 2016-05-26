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
  
  data.frame(as.list(apply(TermDocumentMatrix(my.corpus), 1, sum)))
}

ShortVectoLong <- function(long_term, short_vec){
  loc <- which(long_term %in% colnames(short_vec))
  loc_word <- long_term[loc]
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == colnames(short_vec))] ) 
  }
  
  long_vec
}

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
for(j in 2:length(catego)){
  all_term <- union(all_term, colnames(term_vec[[j]]))
}

### generate term document matrix
tdm <- as.data.frame(matrix(NA, nrow = length(all_term), ncol = length(catego)))
for(k in 1:length(catego)){
  long_vec <- ShortVectoLong(all_term, term_vec[[k]])
  tdm[,k] <- long_vec / sum(long_vec)
}
View(tdm)

### example in textbook
# prob_LM1 = c(0.5, 0.3, 0.1, 0.1)
# prob_LM2 = c(0.2, 0.1, 0.5, 0.3)
# df = data.frame(prob_LM1, prob_LM2)
# word_freq = c(4, 2, 4, 2)
# tau <- c(0.5, 0.5)

# for(i in 1:30){
#   z1 <- (tau[1]*df[,1]) / (tau[1]*df[,1] + tau[2]*df[,2])
#   z2 <- (tau[2]*df[,2]) / (tau[1]*df[,1] + tau[2]*df[,2])
#   tau[1] <- sum(word_freq * z1) / sum(word_freq)
#   tau[2] <- sum(word_freq * z2) / sum(word_freq)
#     
#   log_like <- sum(word_freq * log(tau[1]*df[,1] + tau[2]*df[,2]))
#   print(log_like)
# }

### EM Algorithm
all_test <- DirSource("Test")
text <- readLines(all_test$filelist[1])
query_term <- GetTermVector(VectorSource(text))

df <- tdm
word_freq <- ShortVectoLong(all_term, query_term)
tau <- rep(1/length(catego), length(catego))
TPWM <- as.data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(df)))

for(p in 1:10){
  for(j in 1:ncol(df)){ 
    TPWM[,j] <- tau[j]*df[,j] / apply(df,1, function(x)sum(tau*x)) 
  }
  tau <- apply(TPWM, 2, function(x)sum(word_freq*x) / sum(word_freq))
  log_like <- sum(mapply( function(x,y) y*log(sum(tau*x)), 
                          as.data.frame(t(df)), 
                          as.list(word_freq)))
  print(log_like)
}

print(catego[ which(tau == max(tau)) ])








