# --------------------------------------------------------------------------------
# rm(list=ls(all=T))
# gc()
# --------------------------------------------------------------------------------
source('lib/common.R', chdir=T)
source('lib/textutils.R')
source('lib/mlutils.R')

library(Matrix)
library(e1071)
library(glmnet)
library(caret)
library(qdap) # sentiment polarity: polarity()


#library(doMC)
#registerDoMC(cores = 4)

library(doParallel)
#cl = makeCluster(detectCores())


# Required Files
# --------------------------------------------------------------------------------
tweets_15_tr = paste0(data_dir, "sentiment/TASS/general-tweets-train-tagged.xml")
tweets_15_te = paste0(data_dir, "sentiment/TASS/general-tweets-test-tagged.xml")
tweets_13_te = paste0(data_dir, "sentiment/TASS/politics2013-tweets-test-tagged.xml")

stops1_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_1_es.txt")
stops2_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_2_es.txt")

stops = stop_words(c(stops1_fnm, stops2_fnm), remove_accents=T, include_tm_lang="es")

# TODO: ADD other words not to be removed, like bueno, malo, mucho, bien etc... !!!!

include_stops = c("RT", "RT:")
stops = stops[-which(stops %in% exclude_stops)]
stops = sort(union(stops, include_stops))


# Load tweets and preprocess
# --------------------------------------------------------------------------------
remove_non_sents = TRUE # Remove tweets without sentiment?
remove_non_es = TRUE    # Remove tweets not in Spanish?
remove_neu_sents = TRUE # Remove tweets with neutral sentiment (leaving only POS and NEG)?

merge_tweet_sources = T
merge_train_only = T   # Adds diff. sources for training, but keeps original validation set untouched
equalize_source_sizes = F # WARNING: Throws away a lot of data !
validation_prop = 0.2

df = load_tass_tweets(c(tweets_15_tr, tweets_15_te), remove_non_sents, remove_non_es, remove_neu_sents, shuffle=T)
df_val = load_tass_tweets(tweets_13_te, remove_non_sents, remove_non_es, remove_neu_sents, shuffle=T)

# Otherwise one source may dominate another
if(equalize_source_sizes) {
  # Only take as much samples from bigger data source as in smaller source
  df = df[sample(nrow(df_val)), ]
}

# To get more variety in kinds of tweets
if (merge_tweet_sources) {
  df = rbind(df, df_val)
  df = df[sample(nrow(df)), ] # shuffle just in case
  if(!merge_train_only) {
    train_index = as.numeric(createDataPartition(df$sent_bin, p=(1-validation_prop), list=F, times=1))
    df_val = df[-train_index,]
    df = df[train_index,]
  }
}

# Down-sample training date only for balanced classes, needs shuffle afterwards !!!
prop.table(table(df$sent_bin))
df = downSample(x=df[, -ncol(df)], y=df$sent_bin, yname="sent_bin")
df = df[sample(nrow(df)), ] # shuffle
prop.table(table(df$sent_bin))
N = nrow(df)
N_val = nrow(df_val)
N

sapply(emo_pos_str, function(e) sum(grepl(e, df$text)))
sapply(emo_neg_str, function(e) sum(grepl(e, df$text)))

common_preprocess = function(text) {
  text = substitute_emoticons(text)
  text = remove_twitter_entities(text)
  text = remove_twitter_urls(text)
  text = remove_repeated_char(text, 3)
  #text = remove_accents(text)
}

df$text = common_preprocess(df$text)
df_val$text = common_preprocess(df_val$text)

sum(grepl("emopolpos", df$text))
sum(grepl("emopolneg", df$text))


# Configuration
# --------------------------------------------------------------------------------
remove_sparse_prop = 0.999  # Remove terms absent (count=0) in this proportion of documents

minDocFreq = 1
maxDocFreq = Inf
stopwords = TRUE # stops
ngrams = 1

tm_control = list(
  bound = list(local=c(minDocFreq, maxDocFreq)),
  language = "es",
  tolower = TRUE,  
  removeNumbers = FALSE,
  removePunctuation = TRUE,
  stopwords = stopwords,
  stripWhiteSpace = TRUE,
  wordLengths = c(2, Inf),
  weighting = weightTf,
  stemming = TRUE,
  dictionary = NULL,
  verbose = T
)

if (ngrams > 1) {
  tm_control$tokenize = ngrams_tokenizer(ngrams, package="RWeka")  
} else {
  tm_control$tokenize = "words"
}


# Create DTM
# --------------------------------------------------------------------------------
# Test-tokenize, remove docs that would be empty as result, and tokenize again for real
#   empty_docs = which_empty_docs(corp, tf_ctrl)
#   if (length(empty_docs) > 0) {
#     labels = labels[-empty_docs]
#     corp = corp[-empty_docs]
#   }

corp = Corpus(VectorSource(df$text), readerControl=list(language=tm_control$language))
corp_val = Corpus(VectorSource(df_val$text), readerControl=list(language=tm_control$language))
labels = factor(df$sent_bin)
labels_val = factor(df_val$sent_bin)
dtm_full = DocumentTermMatrix(corp, control=tm_control)
if (remove_sparse_prop < 1) {
  dtm_sm = removeSparseTerms(dtm_full, remove_sparse_prop)
  dtm = dtm_sm
} else {
  dtm = dtm_full
}
dtm = dtm_to_csparse(dtm)
dtm_val = dtm_to_csparse(DocumentTermMatrix(corp_val, control=tm_control))
dtm_val = dtm_filter_terms(dtm_val, dtm)

# check result
head(sort(slam::col_sums(dtm[labels=="Neg",]), decr=T), 25)
head(sort(slam::col_sums(dtm[labels=="Pos",]), decr=T), 25)
head(sort(slam::col_sums(dtm_val[labels_val=="Pos",]), decr=T), 25)

idx = which(dimnames(tdm)$Terms == "buenas noches")
inspect(tdm[idx+(-2:2),1:10])

# Any empty docs ?
row_totals = slam::row_sums(dtm)
empty_rows = dtm[row_totals == 0, ]@Dimnames$Docs

# term frequencies
term_freq = slam::col_sums(dtm)
term_freq_val = slam::col_sums(dtm_val)
head(sort(term_freq, decr=T), 25)
head(sort(term_freq_val, decr=T), 25)
#head(sort(term_freq, decr=F), 25)
plot(as.numeric(sort(term_freq, decr=T)))
hist(term_freq)
plot(ecdf(term_freq))
# Check potential tf cutoff point for removing low scoring ngrams, i.e. sparse terms
median(term_freq)
which(term_freq == median(term_freq))
qs = quantile(term_freq, probs=c(0.25, 0.5, 0.75))
sum(term_freq > qs[2]) / length(term_freq) # Proportion of terms greater than quantile


# Caching
# --------------------------------------------------------------------------------
cache_fnm = "../data/parties/r-cache/sentiment_data.Rdata"
save(df, empty_docs, corp, tdm, dtm, dtm2, file=cache_fnm)
load(cache_fnm)

# Inspect topics (only works if weighting is tf, not tf-idf)
# --------------------------------------------------------------------------------
lda = LDA(dtm, k=5)
term = terms(lda, 6)
term
topic = topics(lda, 1)

# Train classifiers
# --------------------------------------------------------------------------------
train_prop = 0.8
part_random = T

if (part_random) {
  trainIndex = as.numeric(createDataPartition(labels, p=train_prop, list=F, times=1))
} else {
  trainIndex = 1:(train_prop * length(labels))
}

x_tr = dtm[trainIndex, ]
y_tr = labels[trainIndex]
x_te = dtm[-trainIndex, ]
y_te = labels[-trainIndex]

model_svm = svm(x=x_tr, y=y_tr, probability=TRUE, scale=T, type="C-classification", cost=10, gamma=1/ncol(x_tr), cross=0, kernel="radial")
svm_pred = predict_perf_bin(model_svm, x_te, y_te, probability=T)
svm_pred = predict_perf_bin(model_svm, dtm_val, labels_val, probability=T)

# Tune svm with e1071 package
# best cost >= 1000, gamma very small?
# Default gamma = 1/#features = 1/ncol(tm_dtm) = 1.78e-05
tune_control = tune.control(nrepeat=1, sampling="cross", cross=10)
tuned_svm = tune("svm", tm_dtm, labels, kernel="radial", scale=T, probability=TRUE,
                 ranges = list(cost=c(100), gamma=c(2e-10, 2e-5, 2e-1)),
                 tunecontrol=tune_control)

svmt_pred = predict_perf_bin(tuned_svm, x_te, y_te, probability=T)
svmt_pred = predict_perf_bin(tuned_svm, dtm_val, labels_val, probability=T)

# Logistic regression with glmnet
cl = makeCluster(4)
registerDoParallel(cl)
#model_glm_cv = cv.glmnet(x_tr, y_tr, family="binomial", alpha=0.5, type.measure="class", nfolds=10, parallel=T)
model_glm_cv = cv.glmnet(dtm, labels, family="binomial", alpha=0.5, type.measure="class", nfolds=10, parallel=T)
stopCluster(cl)

glm_pred = predict_perf_bin(model_glm_cv, x_te, y_te, type="class", s=model_glm_cv$lambda.min)
glm_pred = predict_perf_bin(model_glm_cv, dtm_val, labels_val, type="class", s=model_glm_cv$lambda.min)

# --------------------------------------------------------------------------------
