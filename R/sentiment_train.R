# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')
library(XML)
library(stringr)
library(stringi)

library(tm)
library(RTextTools)
library(e1071)

# Required Files
# --------------------------------------------------------------------------------
tweets_15_tr = paste0(data_dir, "sentiment/TASS/general-tweets-train-tagged.xml")
tweets_15_te = paste0(data_dir, "sentiment/TASS/general-tweets-test-tagged.xml")
tweets_13_te = paste0(data_dir, "sentiment/TASS/politics2013-tweets-test-tagged.xml")

stops1_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_1_es.txt")
stops2_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_2_es.txt")

stops1 = stri_trans_general(scan(stops1_fnm, what="character"), "Latin-ASCII")
stops1 = stops1[-which(stops1=="podemos")] # Don't exclude podemos !!!
stops2 = stri_trans_general(scan(stops2_fnm, what="character"), "Latin-ASCII")
stops2 = stops2[-which(stops2=="podemos")] # Don't exclude podemos !!!
stopstm = sort(stri_trans_general(stopwords(kind="es"), "Latin-ASCII"))
stopstm = stopstm[-which(stopstm=="podemos")] # Don't exclude podemos !!!
stops = sort(union(stops1, stops2))
stopsall = sort(union(stops, stopstm))

# Load tweets and preprocess
# --------------------------------------------------------------------------------
xml = xmlParse(tweets_15_tr)

ids = sapply(xml["//tweetid"], xmlValue)
sentiment = sapply(xml["/tweets/tweet/sentiments/polarity[1]/value"], xmlValue)
content = sapply(xml["//content"], xmlValue)
lang = sapply(xml["//lang"], xmlValue)

tsdf = data.frame(id=ids, s=sentiment, c=content, l=lang)
tsdf$c = as.character(tsdf$c)
tsdf$id = as.double(as.character(tsdf$id))

# Take only non-None sentiments and only lang=en...
unique(tsdf$s)
unique(tsdf$l)
tsdf = tsdf[tsdf$l == "es", ]
tsdf = tsdf[tsdf$s != "NONE", ]
tsdf = droplevels(tsdf)

# Add numeric polarity
tsdf$sn = revalue(as.character(tsdf$s), c("N+"=-2, "N"=-1, "NEU"=0, "P"=1, "P+"=2))
tsdf$sn = as.numeric(tsdf$sn)
tsdf$sb[tsdf$sn < 0] = -1
tsdf$sb[tsdf$sn == 0] = 0
tsdf$sb[tsdf$sn > 0] = 1

N = nrow(tsdf)

# Clean up tweets
# --------------------------------------------------------------------------------
# 1. emoticons
emo_pos = c(":\\)", ":-\\)", ";\\)", ";-\\)", " :D")
emo_neg = c(":\\(", ":-\\(", ";-\\(", ";\\(")
          
tsdf$c[which(grepl("\\(:", tsdf$c))]

c = tsdf$c
for (e in emo_pos) c = gsub(e, " _emo_p_ ", c)   
for (e in emo_neg) c = gsub(e, " _emo_n_ ", c)   

c[which(grepl("_emo_n_", c))]

# 2. retweets

# 3. mentions


# Train classifier
# --------------------------------------------------------------------------------
# Naive Bayesian on tf-idf
m = create_matrix(tsdf$c, language="spanish", removeStopwords=T, removeNumbers=T)#, stemWords=T)
container = create_container(m, tsdf$sb, trainSize=1:5000, testSize=5001:N, virgin=F)
svm = train_model(container, "SVM")
svm_cl = classify_model(container, svm)

table(svm_cl$SVM_LABEL, tsdf$sb[5001:N])

analy = create_analytics(container, svm_cl)
summary(analy)
analy@label_summary
analy@algorithm_summary
analy@ensemble_summary
analy@document_summary

mm = as.matrix(m)
nb = naiveBayes(mm[1:5000,], factor(tsdf$sb[1:5000]))
pred = predict(nb, mm[5001:N,])
table(pred, tsdf$sb[5001:N])
