library(SnowballC)
library(tm)

# filelist = list.files(pattern = ".*")
# datalist = lapply(filelist, function(x) scan(x, what="character"))

wordStem2 <- function(x) {
  mywords <- unlist(strsplit(x, " "))
  mycleanwords <- gsub("^\\W+|\\W+$", "", mywords, perl=T)
  mycleanwords <- mycleanwords[mycleanwords != ""]
  wordStem(mycleanwords)
}


txt <- "Train/alt.atheism"
my.corpus <- Corpus(DirSource(txt), readerControl = list(language = "lat"))
my.corpus <- tm_map(my.corpus, stripWhitespace)
my.corpus <- tm_map(my.corpus, removePunctuation)
my.corpus <- tm_map(my.corpus, removeNumbers)
my.corpus <- tm_map(my.corpus, content_transformer(tolower))
my.corpus <- tm_map(my.corpus, stemDocument, language = "english")  
my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(my.corpus)

inspect(tdm[1:100, 1:3])

c(apply(tdm,1,sum))





