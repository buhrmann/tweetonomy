# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')
source('textutils.R')

library(Matrix)
library(e1071)
library(glmnet)
library(caret)
library(qdap) # sentiment polarity: polarity()


#library(doMC)
#registerDoMC(cores = 4)

library(doParallel)
#cl = makeCluster(detectCores())


downsample_sparse = function(x, y) { 
  if (!is.factor(y)) {
    warning("Down-sampling requires a factor variable as the response. The original data was returned.")
    return(list(x = x, y = y))
  }
  
  # How many samples in each class?
  min_class_size = min(table(y))
  
  # Attach labels as last column to keep track of correct class when sampling
  # Converts factor levels to numeric, so need to convert back later...
  x = cbind2(x, as.numeric(y))
  
  # Loop over levels l of y, extract indices of corresponding rows of x as rows_l and resample 
  # down to necessary size from these rows. Add sampled indices to vector idx.
  idx = c()
  for (l in unique(as.numeric(y))) {
    rows_l = which(x[, ncol(x)] == l)
    if (length(rows_l) > min_class_size) {      
      idx_l = sample(rows_l, size=min_class_size, replace=FALSE) # Get min_class_size rows
      idx = c(idx, idx_l)
    } else {
      idx = c(idx, rows_l)
    }
  }
  # Now get data from downsampled indices
  x = x[idx, ]
  y_new = x[, ncol(x)] # Extract new labels vector
  y_new = factor(y_new, labels=levels(y)) # Convert back to factor
  x = x[, -ncol(x)] # Remove class labels from x again  
  list(x=x, y=y_new) 
}


upsample_sparse = function(x, y, shuffle=T) {  
  if (!is.factor(y)) {
    warning("Up-sampling requires a factor variable as the response. The original data was returned.")
    return(list(x = x, y = y))
  }
  
  # How many samples in each class?
  max_class_size = max(table(y))
  
  # Attach labels as last column to keep track of correct class when duplicating
  # Converts factor levels to numeric, so need to convert back later...
  x = cbind2(x, as.numeric(y))

  # Loop over levels l of y, extract all corresponding rows of x as x_l and resample 
  # up to necessary size from these rows. Add new rows to matrix.
  for (l in unique(as.numeric(y))) {
    rows_l = x[, ncol(x)] == l
    x_l = x[rows_l, ]
    n = nrow(x_l)
    if (n < max_class_size) {
      ind = sample(1:n, size=max_class_size-n, replace=TRUE) # Get extra rows
      x = rbind2(x, x_l[ind, ])
    }
  }
  
  if (shuffle) {
    rid = sample(nrow(x))
    x = x[rid,]
  }
  
  y_new = x[, ncol(x)] # Extract new labels vector
  y_new = factor(y_new, labels=levels(y))
  x = x[, -ncol(x)] # Remove class labels from x again  
  list(x=x, y=y_new)
}


# Required Files
# --------------------------------------------------------------------------------
tweets_15_tr = paste0(data_dir, "sentiment/TASS/general-tweets-train-tagged.xml")
tweets_15_te = paste0(data_dir, "sentiment/TASS/general-tweets-test-tagged.xml")
tweets_13_te = paste0(data_dir, "sentiment/TASS/politics2013-tweets-test-tagged.xml")

stops1_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_1_es.txt")
stops2_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_2_es.txt")

stops = stop_words(c(stops1_fnm, stops2_fnm), remove_accents=T, include_tm_lang="es")

# TODO: ADD other words not to be removed, like bueno, malo, mucho, bien etc... !!!!
exclude_stops = c("podemos", "bien", "bastante", "buen", "buena", "buenas", "bueno", "buenos", "grandes",
                  "igual", "mas", "mayor", "mejor", "menos", "mucha", "muchas", "mucho", "muchos", "muy",
                  "nada", "nadie", "ni", "ningun", "ninguna", "ningunas", "ninguno", "ningunos", "no",
                  "nunca", "pasada", "sin", "siempre", "sola", "solamente", "solas", "solo", "solos",
                  "tampoco", "tan", "tanto", "toda", "todas", "todavia", "todo", "todos", "total",
                  "ultima", "ultimas", "ultimo", "ultimos", "valor", "verdad", "verdadera", "verdadero")
include_stops = c("RT", "RT:")
stops = stops[-which(stops %in% exclude_stops)]
stops = sort(union(stops, include_stops))

# Load tweets and preprocess
# --------------------------------------------------------------------------------
df = load_tass_tweets(tweets_15_tr)
N = nrow(df)
N

# Convert emoticons to unique non-puntuated tokens
df$text[which(grepl(":)", df$text))]
df$text = substitute_emoticons(df$text)
df$text[which(grepl("emopolpos", df$text))]
df$text[which(grepl("emopolneg", df$text))]

# Remove retweet tokens
df$text = remove_twitter_entities(df$text)

# Remove characters repeated more than 3 times
# But keep at least 3 to potentially detect exaggerations !
df$text = remove_repeated_char(df$text, 3)

# Create corpus and clean up
# --------------------------------------------------------------------------------
corp = preprocessed_corpus(df$text, stops=stops, stemlang=NULL, remove_accents=T)

# 3. mentions

# 4. Repeated letters (exaggerations: aaaaahhh!)


# Create term-document matrix
# --------------------------------------------------------------------------------
tokenizer = ngrams_tokenizer(2, package="NLP")
tf_ctrl = list(tokenize = tokenizer, 
            weighting = function(x) weightTfIdf(x, normalize=T))

# Tokenize, remove docs empty as result, and tokenize again
empty_docs = which_empty_docs(corp, tf_ctrl)
corp = corp[-empty_docs]
tdm = TermDocumentMatrix(corp, control=tf_ctrl)

# check result
tscore = apply(tdm, 1, sum)  # for each term, the total tf-idf score summed over all documents
head(sort(tscore, decr=T), 25)

idx = which(dimnames(tdm)$Terms == "podemos")
inspect(tdm[idx+(-2:2),1:10])

# term frequencies
term_freq = slam::row_sums(tdm)
head(sort(term_freq, decr=T), 25)
head(sort(term_freq, decr=F), 25)
plot(as.numeric(sort(term_freq, decr=T)))
hist(term_freq)
plot(ecdf(term_freq))
median(term_freq)
which(term_freq == median(term_freq))
qs = quantile(term_freq, probs=c(0.25, 0.5, 0.75))
sum(term_freq > qs[1]) / length(term_freq) # Proportion of terms greater than quantile

dtm = as.DocumentTermMatrix(tdm, control=tf_ctrl)

# Remove sparse terms
freq_terms_dict = findFreqTerms(dtm, lowfreq=quantile(term_freq, probs=0.25))
100 * length(freq_terms_dict) / nrow(tdm) # Percentage of original terms
tf_ctrl2 = tf_ctrl
tf_ctrl2$dictionary = freq_terms_dict
dtm2 = DocumentTermMatrix(corp, control=tf_ctrl2)
dtm2 = as.matrix(dtm2)
N = dim(dtm)[1]

# Inspect topics (only works if weighting is tf, not tf-idf)
# --------------------------------------------------------------------------------
lda = LDA(dtm, k=5)
term = terms(lda, 6)
term
topic = topics(lda, 1)


# Train classifiers manually
# --------------------------------------------------------------------------------
sm = Matrix(as.matrix(dtm), sparse=T)
dm = as.matrix(dtm)
labels = factor(df$sent_bin[-empty_docs])

# Equally distributed classes? No!!!
table(labels)

#weights = 1000 * 1/table(labels);
weights = list("-1"=1, "0"=1000, "1"=1)
weights = list("-1"=1, "0"=1, "1"=1)

# Create training and test set
x_tr = dm[1:N_tr,]
y_tr = labels[1:N_tr]
x_te = dm[(N_tr+1):N,]
y_te = labels[(N_tr+1):N]


ctrl = trainControl(#method="repeatedcv", number=5, repeats=1,
                    #method="cv", number=5, repeats=1,
                    method="none",
                    classProbs=TRUE, summaryFunction = multiClassSummary,
                    sampling="up", verboseIter=T, allowParallel=T)

mod = train(x_tr, y_tr, method="glmnet", family="multinomial",
            metric="Accuracy", trControl=ctrl)

# SVM
mod_svm = svm(sm[1:N_tr,], labels[1:N_tr], kernel="linear", cost=1, class.weights=weights)
pred_svm = predict(mod_svm, sm[(N_tr+1):N,])
table(pred_svm, labels[(N_tr+1):N])

# Naive Bayes
options(mc.cores=6)
mod_nb = naiveBayes(dm[1:N_tr,], labels[1:N_tr])
pred_nb = predict(mod_nb, data.frame(dm[(N_tr+1):N,]), type="class")
table(pred_nb, labels[(N_tr+1):N])

# logistic regression
# todo: only upsample training data, i.e. split test data off before...
sample_balance = "up"
cl = makeCluster(5)
registerDoParallel(cl)
trainIndex = as.numeric(createDataPartition(labels, p=0.5, list=F, times=1))
x_tr = sm[trainIndex, ]
y_tr = labels[trainIndex]
x_te = sm[-trainIndex, ]
y_te = labels[-trainIndex]
table(y_tr)
prop.table(table(y_tr))

if (sample_balance %in% c("up", "down")) {
  if (sample_balance == "up") {
    smpl = upsample_sparse(x_tr, y_tr, shuffle=T)
  }
  else if (sample_balance == "down") {
    smpl = downsample_sparse(x_tr, y_tr)
  }
  x_tr = smpl$x
  y_tr = smpl$y
}

prop.table(table(labels))
table(y_tr)
prop.table(table(y_tr))
prop.table(table(y_te))

#mod_glm = glmnet(x_tr, y_tr, family="multinomial")
#pred_glm_p = predict(mod_glm, newx=x_te, type="response", s=opt_lambda)

# alpha=0: ridge, alpha=1: lasso
glm_cv = cv.glmnet(x_tr, y_tr, family="multinomial", type.measure="deviance", nfolds=10, parallel=T, alpha=0.0)
pred_glm_c = predict(glm_cv, newx=x_te, type="class", s=glm_cv$lambda.min)
table(pred_glm_c, y_te)

predf = data.frame(obs=y_te, pred=pred_glm_c[,1])
multiClassSummary(predf, lev=levels(labels))
confusionMatrix(predf$pred, predf$obs)

stopCluster(cl)

# Train classifiers with RTextTools
# --------------------------------------------------------------------------------
# Naive Bayesian on tf-idf
#dtm = create_matrix(tsdf$c, language="spanish", removeStopwords=T, removeNumbers=T)#, stemWords=T)
container = create_container(dtm, df$sent_bin[-empty_docs], trainSize=1:5000, testSize=5001:N, virgin=F)
svm = train_model(container, "SVM")
svm_cl = classify_model(container, svm)

table(svm_cl$SVM_LABEL, tsdf$sb[5001:N])

analy = create_analytics(container, svm_cl)
summary(analy)
analy@label_summary
analy@algorithm_summary
analy@ensemble_summary
analy@document_summary

