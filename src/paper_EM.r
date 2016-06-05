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
  
  if(length(loc)==0){ return(rep(1,length=length(long_term))) }
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == names(short_vec))] ) 
  }
  
  return(long_vec)
}


##### main #####

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file)", call.=FALSE)
} else if (length(args) < 3) {
  used_label_count <- -1
} else{
  used_label_count <- as.numeric(args[3])
}

setwd(args[1]) # setwd("~/Desktop/text-mining-in-EM-algorithm/test")
out_name <- args[2]

# setwd("~/Desktop/text-mining-in-EM-algorithm/test")
#


Rcpp::sourceCpp("../src/tm.cpp")

all_time = Sys.time()


### generate term freq. in each topic
catego <- list.dirs('Train', recursive=FALSE)

term_vec <- list()
all_term <- c()

for(i in 1: length(catego)){
  file <- DirSource(catego[i])
  if(used_label_count!=-1){
    if(file$length > used_label_count){
      file$filelist <- file$filelist[1:used_label_count]
      file$length <- used_label_count
    }
  }
  term_vec[[i]] <- GetTermVector(file)
  all_term <- union(all_term, names(term_vec[[i]]))
}


### generate term freq. for unlabeled
file_unlabel <- DirSource("Unlabel")
used_unlabel_count <- -1

if(used_unlabel_count!=-1){
  file_unlabel$filelist <- file_unlabel$filelist[1:used_unlabel_count]
  file_unlabel$length <- used_unlabel_count
}

term_vec_unlabel <- list()
all_term_unlabel <- c()

for(i in 1:length(file_unlabel)){
  term_vec_unlabel[[i]]  <- GetTermVectorCpp(file_unlabel$filelist[i])
  all_term_unlabel <- union(all_term_unlabel, names(term_vec_unlabel[[i]]))
}


### generate all term set 
all_term <- union(all_term, all_term_unlabel)


### generate term document matrix and all term freq
term_freq <- vector(mode = "numeric", length = length(all_term))

tdm <- matrix(NA, nrow = length(all_term), ncol = length(catego))
for(i in 1:length(catego)){
  long_vec <- ShortVectoLong(all_term, term_vec[[i]])
  tdm[,i] <- long_vec
  term_freq <- term_freq + long_vec
}

tdm_unlabel <- matrix(NA, nrow = length(all_term), ncol = length(file_unlabel))
for(i in 1:length(file_unlabel)){
  long_vec <- ShortVectoLong(all_term, term_vec_unlabel[[i]])
  tdm_unlabel[,i] <- long_vec
  term_freq <- term_freq + long_vec
}


### kill low freq term
low_freq_term <- which(term_freq<=(sort(term_freq)[1])+1)
all_term <- all_term[-low_freq_term]
tdm <- tdm[-low_freq_term,]
tdm_unlabel <- tdm_unlabel[-low_freq_term,]


### calculate prob of word in a topic
all_wordcount_in_a_topic <- apply(tdm,2,sum)
word_in_a_class_prob <- log2(t(apply(tdm,1,function(x) (0.0001 + x) / (all_wordcount_in_a_topic))))
# note : apply - for one word in all each topic int one thread(row)

topic_doc_count <- c()
for(i in 1:length(catego)){
  file <- DirSource(catego[i])
  if(used_label_count!=-1){
    if(file$length > used_label_count){
      file$filelist <- file$filelist[1:used_label_count]
      file$length <- used_label_count
    }
  }
  topic_doc_count <- c(topic_doc_count, length(file))
}
D_len <- sum(topic_doc_count)
class_prior_prob <- log2( (0.0001 + topic_doc_count) / (D_len) )

# backup for smoothing
#  - V_len <- length(all_term)
#  - C_len <- length(catego)
#  - word_in_a_class_prob <- log2(t(apply(tdm_after,1,function(x) (0.1+x) / (V_len+all_wordcount_in_a_topic))))
#  - class_prior_prob <- log2( (1+topic_doc_count_after) / (C_len+D_len) )


## EM algorithm for unlabeled data
for(times in 1:6){

  ans_list_unlabel <- vector(mode="character",length=length(file_unlabel))
  for(i in 1:length(file_unlabel)){
    query_term_long <- tdm_unlabel[,i]
    log_for_product <- apply(word_in_a_class_prob, 2,
                             function(x) sum(query_term_long * x)) + class_prior_prob
    ans_list_unlabel[i] <- catego[ which(log_for_product == max(log_for_product)) ]

    cat( paste(i, ans_list_unlabel[i], collapse = " "), "\n")
  }

  ### generate term document matrix
  tdm_after <- tdm
  topic_doc_count_after <- topic_doc_count

  for(i in 1:length(file_unlabel)){
    catego_unlabel <- which(catego == ans_list_unlabel[i])
    tdm_after[,catego_unlabel] <- tdm_after[,catego_unlabel] + tdm_unlabel[,i]
    topic_doc_count_after[catego_unlabel] <- topic_doc_count_after[catego_unlabel] + 1
  }

  all_wordcount_in_a_topic <- apply(tdm_after,2,sum)
  word_in_a_class_prob <- log2(t(apply(tdm_after,1,function(x) (0.0001 + x) / (all_wordcount_in_a_topic))))

  D_len <- sum(topic_doc_count_after)
  class_prior_prob <- log2( (0.0001 + topic_doc_count_after) / (D_len))
}


# labeled size = 20		0.53891071239			0.680114661854	(#iter = 7)

### handle query document 
all_test <- DirSource('Test')
all_test <- lapply(all_test$filelist, function(x)substring(x,6,9))
all_test <- paste0(rep('Test/',length(all_test)), 
                   as.character(sort(as.numeric(all_test))))

### Naive Algorithm
ans_list <- vector(mode="character",length=length(all_test))
# names(ans_list) <- 1:length(all_test)


all_ranking_list <- data.frame() 
for(i in 1:length(all_test)){
  query_term <- GetTermVectorCpp(all_test[i])
  query_term_long <- ShortVectoLong(all_term, query_term)
  
  ### Using a Classifier
  log_for_product <- apply(word_in_a_class_prob, 2, function(x) sum(query_term_long * x)) + class_prior_prob
  ans_list[i] <- catego[ which(log_for_product == max(log_for_product)) ]
  
  cat( paste(i, ans_list[i], collapse = " "), "\n")
  
  ans.out <-cbind(i, ans_list[i])
  all_ranking_list <- rbind(all_ranking_list,ans.out)
}

out_dir <- paste("../out/", out_name, sep="")
write.table(all_ranking_list, file=out_dir, sep = " ",
            row.names = FALSE, col.names = FALSE, quote = FALSE)

given_ans <- read.table("ans.test.txt")
given_ans <- as.vector(t(given_ans[2]))

cat("precision : ", sum(given_ans == paste0(lapply(ans_list, function(x)substring(x,7,40)))) / 9419 , "\n")
cat("time : ", Sys.time() - all_time, "\n")




