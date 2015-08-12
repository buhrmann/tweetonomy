# http://www.slideshare.net/rdatamining/text-mining-with-r-an-analysis-of-twitter-data
library(tm)
library(SnowballC)
library(wordcloud)
library(fpc)
library(topicmodels)

colnames = c("id", "text")
tweets = read.csv("~/Code/tweetonomy/data/tweets-20150727.txt", sep='\t', header=F, 
                  col.names=colnames, quote="", stringsAsFactors=F,
                  comment.char="")

corp = Corpus(VectorSource(tweets$text))
corp = tm_map(corp, content_transformer(tolower))
corp = tm_map(corp, removePunctuation)
corp = tm_map(corp, removeNumbers)

removeUrl = function(x) gsub("http[[:alnum:]]*", "", x)
corp = tm_map(corp, content_transformer(removeUrl))

stops = c(stopwords("english"), "rt")
corp = tm_map(corp, removeWords, stops)

orig_corp = corp
corp = tm_map(corp, stemDocument, language="english")
corp = tm_map(corp, stripWhitespace)

# Counting
#sum(unlist(tm_map(corp, content_transformer(grep), pattern="\\<varoufakis")))

# Remove tweets empty after rpreprocessing
# https://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
dtm = DocumentTermMatrix(corp)
rowtotal = apply(dtm, 1, sum)
empty_rows = dtm[rowtotal == 0, ]$dimnames$Docs
corp = corp[-as.numeric(empty_rows)]

tdm = TermDocumentMatrix(corp)
idx = which(dimnames(tdm)$Terms == "varoufaki")
inspect(tdm[idx+(-2:2),1:10])

# Frequent words
freq_terms = findFreqTerms(tdm, lowfreq=300)
print(freq_terms)

# Associations
findAssocs(tdm, "varoufaki", 0.2)

# Word cloud
m = as.matrix(tdm)
word_freq = sort(rowSums(m), decr=T)
wordcloud(words=names(word_freq), freq=word_freq, min.freq=300, random.order=F)

# Cluster words
sparse_thresh = 0.98
tdm2 = removeSparseTerms(tdm, sparse=sparse_thresh)
m2 = as.matrix(tdm2)
distMat = dist(scale(m2))
fit = hclust(distMat, method="ward.D")
plot(fit)
rect.hclust(fit, k=6)

# Cluster documents using kmeanskm$center
set.seed(122)
k = 6
m3 = t(m2)
km = kmeans(m3, k)
round(km$centers, digits=3) # cluster centers

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s = sort(km$centers[i,], decr=T)
  cat(names(s)[1:10], "\n")
  #print the tweets of every cluster
  #print(tweets[which(km$cluster==i)])
}

# Partitioning around medoids (too slow... don;t execute)
#pamr = pamk(m3, metric="manhattan")
#k = pamr$nc
#...

# Topc modelling
dtm = as.DocumentTermMatrix(tdm)
lda = LDA(dtm, k=5)
term = terms(lda, 6)
term
topic = topics(lda, 1)
