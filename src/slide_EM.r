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
  if(length(loc)==0) return(rep(1,length=length(long_term)))
  
  long_vec <- vector(mode="integer",length=length(long_term))
  for(i in 1:length(loc)){
    long_vec[loc[i]] <- as.numeric( short_vec[which(loc_word[i] == names(short_vec))] ) 
  }
  
  return(long_vec)
}

# EM_Algorithm <- function(word_freq, tau, tdm){
#   TPWM <- as.data.frame(matrix(NA, nrow = nrow(tdm), ncol = ncol(tdm)))
#   for(i in 1:8){
#     denom <- apply(tdm,1, function(x)sum(tau*x))
#     for(j in 1:ncol(tdm)){ 
#       TPWM[,j] <- tau[j]*tdm[,j] / denom
#     }
#     
#     tau <- apply(TPWM, 2, function(x)sum(word_freq*x) / sum(word_freq))
#     
#     # log_like <- sum(mapply( function(x,y) y*log(sum(tau*x)), 
#     #                         as.data.frame(t(tdm)), 
#     #                         as.list(word_freq)))
#     # print(log_like)
#   }
#   
#   catego[ which(tau == max(tau)) ]
# }

##### main #####

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("At least one argument must be supplied (input file)", call.=FALSE)
}else if (length(args) < 3) {
  used_label_count <- -1
}else{
  used_label_count <- as.numeric(args[3])
}

setwd(args[1]) # setwd("~/Desktop/text-mining-in-EM-algorithm/test")
out_name <- args[2]

Rcpp::sourceCpp('../src/EM.cpp')
Rcpp::sourceCpp('../src/tm.cpp')

all_time = Sys.time()

### generate term freq. in each topic and All term set 
catego <- list.dirs('Train', recursive=FALSE)

term_vec <- list()
all_term <- c()

for(i in 1: length(catego)){
  file <- DirSource(catego[i])
  if(used_label_count!=-1){
    file$filelist <- file$filelist[1:used_label_count]
    file$length <- used_label_count
  }
  term_vec[[i]] <- GetTermVector(file)
  all_term <- union(all_term, names(term_vec[[i]]))
}


### generate term document matrix
tdm <- matrix(NA, nrow = length(all_term), ncol = length(catego))
for(i in 1:length(catego)){
  long_vec <- ShortVectoLong(all_term, term_vec[[i]])
  tdm[,i] <- long_vec / sum(long_vec)
}


### kill low freq term
low_freq_term <- which(apply(tdm,1,sum)<=2/sum(long_vec))
all_term <- all_term[-low_freq_term]
tdm <- tdm[-low_freq_term,]


### handle query document 
all_test <- DirSource('Test')
all_test <- lapply(all_test$filelist, function(x)substring(x,6,9))
all_test <- paste0(rep('Test/',length(all_test)), 
                   as.character(sort(as.numeric(all_test))))
all_test <- all_test


### EM Algorithm
ans_list <- vector(mode="character",length=length(all_test))
all_ranking_list <- data.frame() 

for(i in 1:length(all_test)){
  # query_term <- GetTermVector(VectorSource(readLines(all_test[i])))
  query_term <- data.frame(as.list(GetTermVectorCpp(all_test[i])))
  
  final_tau <- EM_AlgorithmCpp(word_freq = ShortVectoLong(all_term, query_term),
                                 tau = rep(1/length(catego), length(catego)),
                                 tdm = tdm
                                )
  
  ans_list[i] <- catego[ which(final_tau == max(final_tau)) ]
  # ans_list[i] <- EM_Algorithm(word_freq = ShortVectoLong(all_term, query_term),
  #                             tau = rep(1/length(catego), length(catego)),
  #                             tdm = tdm
  #                            )
  
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




### benchmark
# install.packages("rbenchmark")
# library("rbenchmark")
# benchmark(meanC(x),mean(x),
#           columns=c("test", "replications",
#                     "elapsed", "relative"),
#           order="relative", replications=10000)

# R Compiler 套件：加速 R 程式碼的執行速度
# http://blogger.gtwang.org/2011/08/r-compiler-r-r-compiler-package-speed.html
# https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-quickref.pdf
# http://www.kamalnigam.com/papers/emcat-mlj99.pdf


