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

### generate term document matrix
tdm <- as.data.frame(matrix(NA, nrow = length(all_term), ncol = length(txt)))
for(k in 1:length(txt)){
  tdm[,k] <- ShortVectoLong(all_term, term_vec[[k]])
}
View(tdm)

### example in textbook
prob_LM1 = c(0.5, 0.3, 0.1, 0.1) 
prob_LM2 = c(0.2, 0.1, 0.5, 0.3) 
df = data.frame(prob_LM1, prob_LM2)
word_freq = c(4, 2, 4, 2)
tau <- c(0.5, 0.5)

for(i in 1:30){
  z1 <- (tau[1]*df[,1]) / (tau[1]*df[,1] + tau[2]*df[,2])
  z2 <- (tau[2]*df[,2]) / (tau[1]*df[,1] + tau[2]*df[,2])
  tau[1] <- sum(word_freq * z1) / sum(word_freq)
  tau[2] <- sum(word_freq * z2) / sum(word_freq)
    
  log_like <- sum(word_freq * log(tau[1]*df[,1] + tau[2]*df[,2]))
  print(log_like)
}

### EM Algorithm

for(p in 1:30){
  TPWM <- as.data.frame(matrix(NA, nrow = length(word_freq), ncol = ncol(df)))
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){ 
      TPWM[i,j] <- tau[j]*df[i,j] / sum(tau*df[i,]) 
    }
  }
  for(i in 1:length(tau)){
    tau[i] <- sum(word_freq * TPWM[,i]) / sum(word_freq)
  }
  
  log_like <- 0
  for(i in 1:length(word_freq)){
    log_like <- log_like + word_freq[i]*log(sum(tau*df[i,]))
  }
  print(log_like)
}




