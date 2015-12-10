# ---------------------------------------------------------------------------------------
# rm(list=ls(all=T))
# gc()
# ---------------------------------------------------------------------------------------
source('lib/common.R', chdir=T)
source('lib/textutils.R')
source('lib/mlutils.R')

library(text2vec)
library(Matrix)
library(e1071)
library(glmnet)
library(caret)
library(qdap) # sentiment polarity: polarity()

library(parallel)
library(doMC)
library(doParallel)

# File locations
# ---------------------------------------------------------------------------------------
tweets_15_tr = paste0(data_dir, "sentiment/TASS/general-tweets-train-tagged.xml")
tweets_15_te = paste0(data_dir, "sentiment/TASS/general-tweets-test-tagged.xml")
tweets_13_te = paste0(data_dir, "sentiment/TASS/politics2013-tweets-test-tagged.xml")

stops1_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_1_es.txt")
stops2_fnm = paste0(data_dir, "sentiment/SEPLN-TASS15/DATA/stop-words/stop-words_spanish_2_es.txt")

scorer_dir = paste0(data_dir, "sentiment/scorers/")

# Get text
# ---------------------------------------------------------------------------------------
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

# TODO: allow for ternary sentiment (three classes)
# Down-sample training date only for balanced classes, needs shuffle afterwards !!!
prop.table(table(df$sent_bin))
df = downSample(x=df[, -ncol(df)], y=df$sent_bin, yname="sent_bin")
df = df[sample(nrow(df)), ] # shuffle
prop.table(table(df$sent_bin))
N = nrow(df)
N_val = nrow(df_val)
N

labels = factor(df$sent_bin)
labels_val = factor(df_val$sent_bin)

# Get stop words
stops = stop_words(c(stops1_fnm, stops2_fnm), remove_accents=F, include_tm_lang="es")
stops = stops[-which(stops %in% exclude_stops_es)]
include_stops = c("rt", "rt:", "x", "q", "d")
stops = sort(union(stops, include_stops))

# Setup
# ---------------------------------------------------------------------------------------
# Preprocessing control parameters
ppc = pp_control
ppc$toLower = TRUE  
ppc$convertEmojis = TRUE
ppc$removeUrls = TRUE
ppc$removeTwitterEntities = TRUE
ppc$removePunctuation = TRUE
ppc$removeNumbers = TRUE
ppc$removeRepeatedCharN = 3
ppc$removeAccents = FALSE
ppc$stops = stops # NULL | stops
ppc$tokenizeWords = TRUE
ppc$stemLang = "es" # "es" | NULLL

# Control vectorization: pruning of vocabulary if not using feature hashing
ppc$term_count_min = 10          # 1   for no pruning
ppc$term_count_max = Inf         # Inf for no pruning
ppc$doc_proportion_min = 0.0001   # 0   for no pruning
ppc$doc_proportion_max = 0.4     # 1   for no prunning
ppc$max_number_of_terms = Inf    # Inf for no pruning

# Control vectorization
ppc$ngram_min = 1
ppc$ngram_max = 3
ppc$dtm_transformer = tf_transformer # tf_transformer | tfidf_transformer | binary_transformer | identity
ppc$feature_hashing = FALSE

# Control classifiers
glm_measure = "auc" # "class"

# Tokenize and vectorize
# ---------------------------------------------------------------------------------------
# Get vocabulary and corpus
corp = get_t2v_corpus(df$text, ppc, verbose=T)
corp_val = get_t2v_corpus(df_val$text, ppc, corp$vocab)
vocab_info(corp$vocab, 50)

# Get document-term matrix
dtm = get_t2v_dtm(corp$corpus, ppc)
dtm_val = get_t2v_dtm(corp_val$corpus, ppc)

# Check results
if (ncol(dtm) != ncol(dtm_val)) {
  warning("Different number of features in DTM for training and validation set!")
}

row_totals = slam::row_sums(dtm)
empty_rows = which(row_totals == 0)
empty_tweets = df$text[empty_rows]
empty_tweets

# Train
# ---------------------------------------------------------------------------------------
cl = makeCluster(5)
registerDoParallel(cl)
mod_glm = cv.glmnet(x=dtm, y=labels, family="binomial", alpha=1, type.measure=glm_measure, 
                         nfolds=10, parallel=T, thresh=1e-3, maxit=1e3)
stopCluster(cl)

plot(mod_glm)
print (paste("max AUC = ", round(max(mod_glm$cvm), 4)))

pred_glm = predict_perf_bin(mod_glm, dtm_val, labels_val, type="class", s=mod_glm$lambda.min)
pred_glm = predict_perf_bin(mod_glm, dtm, labels, type="class", s=mod_glm$lambda.min)

# Store scorer, preprocessing parameters, and vocabulary
vocab = corp$vocab
save(mod_glm, ppc, vocab, file=paste0(scorer_dir, "glm.Rdata"))


# ---------------------------------------------------------------------------------------
# qdap polarity
# ---------------------------------------------------------------------------------------
lexicon = "isol"
if (lexicon == "sol") {
  sent_lex_fnm = paste0(data_dir, "sentiment/sol")
  pos_fnm = paste0(sent_lex_fnm, "/sol_positivas.csv")
  neg_fnm = paste0(sent_lex_fnm, "/sol_negativas.csv")
} else if (lexicon == "isol") {
  sent_lex_fnm = paste0(data_dir, "sentiment/isol")
  pos_fnm = paste0(sent_lex_fnm, "/positivas_mejorada.csv")
  neg_fnm = paste0(sent_lex_fnm, "/negativas_mejorada.csv")
}

# These are without accents. Text needs to be stripped too !!!
# qdapDictionaries::key.pol
pos = data.frame(x=scan(pos_fnm, what="character"), y=1)
neg = data.frame(x=scan(neg_fnm, what="character"), y=-1)
pol_df_es = rbind(pos, neg)

# qdapDictionaries::negation.words # negaciones
negations = c("no", "nunca", "sin", "ningun", "ninguno", "nada", "tampoco", "nadie", "jamas", "ni", "sin")

# qdapDictionaries::amplification.words # adverbios/adjetivos grado positivo, intensificadores positivos, cuantificadores
amplifications = c("muy", "mas", "mucho", "muchos", "optimo", "bastante", "bien", "buen", "demasiado", "super")

# qdapDictionaries::deamplification.words # adverbios grado negativo, intensificadores negativos
deamplifications = c("menos", "tan", "poco", "poca", "apenas", "casi", "en absoluto", "algo")



i=1
j=nrow(df)
tw = df$text[i:j]
pol = polarity(tw, 
               grouping.var = NULL,
               polarity.frame = pol_df_es, constrain = FALSE,
               negators = negations,
               amplifiers = amplifications,
               deamplifiers = deamplifications,
               question.weight = 0, amplifier.weight = 0.8, n.before = 4,
               n.after = 2, rm.incomplete = FALSE, digits = 3)

pred = as.factor(ifelse(pol$all$polarity < 0, "Neg", "Pos"))
cl = data.frame(pred=pred, sent=df$sent_bin[i:j])
nas = which(is.na(cl$pred))
cl = cl[-nas,]
acc = sum(cl$pred==cl$sent)/nrow(cl)
acc

pol = polarity(tw, 
         grouping.var = NULL,
         polarity.frame = qdapDictionaries::key.pol, constrain = FALSE,
         negators = qdapDictionaries::negation.words,
         amplifiers = qdapDictionaries::amplification.words,
         deamplifiers = qdapDictionaries::deamplification.words,
         question.weight = 0, amplifier.weight = 0.8, n.before = 4,
         n.after = 2, rm.incomplete = FALSE, digits = 3)


# Topics
# ---------------------------------------------------------------------------------------
stm = corpus %>% get_dtm %>% fix_dimnames %>% csparse_to_simpletriplet
row_totals = slam::row_sums(stm)
empty_rows = which(row_totals == 0)
stm = stm[-empty_rows,]

lda = LDA(stm, k=5)
term = terms(lda, 6)
term
topic = topics(lda, 1)
