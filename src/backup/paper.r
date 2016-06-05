library(SnowballC)
library(tm)

GetTermVector <- function(file_dir){
  my.corpus <- Corpus(file_dir, readerControl = list(language = "lat"))
  my.corpus <- tm_map(my.corpus, function(x) gsub("[^[:alnum:]]", " ", x))
  my.corpus <- tm_map(my.corpus, stripWhitespace)
  my.corpus <- tm_map(my.corpus, removePunctuation)
  my.corpus <- tm_map(my.corpus, removeNumbers)
  my.corpus <- tm_map(my.corpus, tolower)
  # # # for some version need transformer : tm_map(my.corpus, content_transformer(tolower))
  # my.corpus <- tm_map(my.corpus, stemDocument, language = "english")
  my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
  my.corpus <- tm_map(my.corpus, PlainTextDocument)
  # # for some version need reset : Corpus(VectorSource(my.corpus))

  return(apply(TermDocumentMatrix(my.corpus), 1, sum))
}

ShortVectoLong <- function(long_term, short_vec){
  loc <- which(long_term %in% names(short_vec))
  loc_word <- long_term[loc]
  
  if(length(loc)==0){ 
    return(rep(1,length=length(long_term)))
  }
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == names(short_vec))] ) 
  }
  
  return(long_vec)
}

##### main #####

all_time = Sys.time()

### set all file path
setwd("~/Desktop/text-mining-in-EM-algorithm/test")
catego <- list.dirs('Train', recursive=FALSE)

### generate term freq. in each topic
used_label_count <- -1
term_vec <- list()
for(i in 1: length(catego)){
  file <- DirSource(catego[i])
  if(used_label_count!=-1){
    file$filelist <- file$filelist[1:used_label_count]
    file$length <- used_label_count
  }
  term_vec[[i]] <- GetTermVector(file)
}

### generate All term set 
all_term <- colnames(term_vec[[1]])
for(i in 2:length(catego)){
  all_term <- union(all_term, names(term_vec[[i]]))
}

### generate term document matrix
tdm <- matrix(NA, nrow = length(all_term), ncol = length(catego))
for(i in 1:length(catego)){
  long_vec <- ShortVectoLong(all_term, term_vec[[i]])
  tdm[,i] <- long_vec
}

### kill low freq term
low_freq_term <- which(apply(tdm,1,sum)<=1)
all_term <- all_term[-low_freq_term]
tdm <- tdm[-low_freq_term,]

### calculate prob of word in a topic
all_wordcount_in_a_topic <- apply(tdm,2,sum)
V_len <- length(all_term)
word_in_a_class_prob <- log2(t(apply(tdm,1,function(x) (1+x) / (V_len+all_wordcount_in_a_topic))))
  # apply : for one word in all each topic int one thread(row)

### calculate prob of word in a topic
topic_doc_count <- c()
for(i in 1:length(catego)){
  topic_doc_count <- c(topic_doc_count, length(DirSource(catego[i])))
}
D_len <- sum(topic_doc_count)
C_len <- length(catego)
class_prior_prob <- (1+topic_doc_count) / (C_len+D_len)

### handle query document 
all_test <- DirSource('Test')
all_test <- lapply(all_test$filelist, function(x)substring(x,6,9))
all_test <- paste0(rep('Test/',length(all_test)), 
                   as.character(sort(as.numeric(all_test))))

### Naive Algorithm
ans_list <- vector(mode="character",length=length(all_test))
# names(ans_list) <- 1:length(all_test)

for(i in 1:length(all_test)){
  query_term <- GetTermVectorCpp(all_test[i])
  query_term_long <- ShortVectoLong(all_term, query_term)
  
  ### Using a Classifier
  log_for_product <- apply(word_in_a_class_prob, 2, function(x) sum(query_term_long * x))
  ans_list[i] <- catego[ which(log_for_product == max(log_for_product)) ]
  
  # print( paste(i, ans_list[i], collapse = " "))
}

given_ans <- read.table("ans.test.txt")
given_ans <- as.vector(t(given_ans[2]))

sum(given_ans == paste0(lapply(ans_list, function(x)substring(x,7,40)))) / 9419

print(Sys.time() - all_time)




