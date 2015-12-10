library(XML)
library(plyr)
library(stringr)
library(stringi)

library(slam)
library(tm)
library(RWeka)
library(NLP)
library(tau)
library(text2vec)
library(SnowballC)

emo_pos_str = c(":\\)", ":-\\)", ";\\)", ";-\\)", " :D")
emo_neg_str = c(":\\(", ":-\\(", ";-\\(", ";\\(")


# Simple triplet matrix as used in tm package to dgCMatrix from Matrix package.
# --------------------------------------------------------------------------------
dtm_to_csparse = function(dtm) {
  sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol),
               dimnames=dtm$dimnames)
}

# slam's simple_triplet_matrix seems to be 1-indexed, and Matrix's csparse 0-indexed ?!
csparse_to_simpletriplet = function(dtm) {
  tmp = as(dtm, "dgTMatrix")
  simple_triplet_matrix(i=tmp@i+1, j=tmp@j+1, v=tmp@x, dimnames=tmp@Dimnames)
}


# Function to ensure a dtm derived from new data has same structure as an original dtm.
# Will remove those terms not in original dtm, and fill columns for terms only in
# original dtm with 0 weights.
# Returns a CSparse matrix from the Matrix package and expects the same type as input.
# ------------------------------------------------------------------------------------
dtm_filter_terms = function(new_dtm, orig_dtm) {
  common_colnames = intersect(colnames(new_dtm), colnames(orig_dtm))
  
  # Create empty matrix with #rows equal to new dataset and #cols of original dataset
  N = nrow(new_dtm)
  dtm = Matrix(0, N, ncol(orig_dtm), dimnames=list(Docs=new_dtm@Dimnames$Docs, Terms=orig_dtm@Dimnames$Terms))
  dtm = as(dtm, "dgCMatrix")
  # Now copy data over. Only numeric indexing allowed though :(
  dtm[, common_colnames] = new_dtm[, common_colnames]
  dtm
}


# --------------------------------------------------------------------------------
ngrams_tokenizer = function(n, package="RWeka") {
  if (package == "RWeka") {
    options(mc.cores=1) # Still necessary with below explicit namesapce?
    tokenizer = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))
  } else if (package == "NLP") {
    tokenizer = function(x) unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "), use.names = FALSE)
  } else if (package == "tau") {
    tokenizer = function(x) rownames(as.data.frame(unclass(textcnt(x, method="string", n=n))))
  }
  tokenizer
}


# Remove urls from tweets
# --------------------------------------------------------------------------------
remove_twitter_urls = function(text) {
  tw_url = "(https?://t\\.co[^ ]*)|(t\\.co[^ ]*)"
  url1 = "(http[^ ]*)|(ftp[^ ]*)|(www\\.[^ ]*)"
  #url2 = "(((https?|ftps?)://)|(www\\.))(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?"
  #url3 = "(https?|ftps?)://(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?"
  res = gsub(paste0(tw_url, "|", url1), "", text)
  res
}


# Remove all accents (e.g. from Spanish text)
# --------------------------------------------------------------------------------
remove_accents = function(text) {
  stri_trans_general(text, "Latin-ASCII")
}


# text could be e.g. a data frame column
# --------------------------------------------------------------------------------
substitute_emoticons = function(text, pos_str="emopolpos", neg_str="emopolneg") {
  for (e in emo_pos_str) text = gsub(e, " emopolpos ", text)
  for (e in emo_neg_str) text = gsub(e, " emopolneg ", text)
  text
}


# Remove RT or "via", and @mentions
# --------------------------------------------------------------------------------
remove_twitter_entities = function(text) {
  text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", text)
  text = gsub("@\\w+", "", text)
  text
}


# Replaces characters repeated more than n times with only n instances
# E.g. n=3: "Buenaaaaaas" -> "Buenaaas"
# --------------------------------------------------------------------------------
remove_repeated_char = function(text, n=3) {
  pat = sprintf('(.)\\1{%i,}', n)
  sub = paste(rep('\\1', n), collapse="")
  gsub(pat, sub, text)
}


# 
# --------------------------------------------------------------------------------
exclude_stops_es = c("podemos", "bien", "bastante", "buen", "buena", "buenas", "bueno", "buenos", "grandes",
                  "igual", "más", "mayor", "mejor", "menos", "mucha", "muchas", "mucho", "muchos", "muy",
                  "nada", "nadie", "ni", "ningún", "ninguna", "ningunas", "ninguno", "ningunos", "no",
                  "nunca", "pasada", "sí", "sin", "siempre", "sola", "solamente", "solas", "solo", "sólo", "solos",
                  "tampoco", "tan", "tanto", "toda", "todas", "todavía", "todo", "todos", "total",
                  "ultima", "última", "ultimas", "últimas", "ultimo", "último", "ultimos", "últimos", 
                  "valor", "verdad", "verdadera", "verdadero")


stop_words = function(fnm_list, remove_accents=T, include_tm_lang=NULL) {
  if (length(fnm_list)==0 && is.null(include_tm_lang)) {
    print("No stopword source defined! Returning NULL.")
    return(NULL)
  }
  
  stops = c()
  if(!is.null(include_tm_lang)) {
    words = stopwords(kind=include_tm_lang)
    if (remove_accents) words = stri_trans_general(words, "Latin-ASCII")
    stops = c(stops, list(words))
  }
  
  stops = c(stops, lapply(fnm_list,  FUN=function(x) {
    words = scan(x, what="character")
    if (remove_accents) words = stri_trans_general(words, "Latin-ASCII")
    words
    })
  )
  
  if(length(stops) > 0) stops = Reduce(union, stops)
  sort(stops)
}


# Build tm corpus from text and preprocess
# --------------------------------------------------------------------------------
preprocessed_corpus = function(text, language="en", stops=NULL, stem=FALSE, remove_accents=T) {
  corp = Corpus(VectorSource(text), readerControl=list(language=language))
  corp = tm_map(corp, content_transformer(tolower))
  corp = tm_map(corp, removePunctuation)
  corp = tm_map(corp, removeNumbers)
  
  corp = tm_map(corp, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  
  if (!is.null(stops)) corp = tm_map(corp, removeWords, stops)
  if (stem) corp = tm_map(corp, stemDocument, language=language)
  if (remove_accents) corp = tm_map(corp, content_transformer(function(x) stri_trans_general(x, "Latin-ASCII")))
  
  corp = tm_map(corp, stripWhitespace)
  corp
}


# Control parameters for 
# Note: stemming only works if text is also tokenized first.
# --------------------------------------------------------------------------------
pp_control = list(
  # Preprocessing
  toLower = FALSE, 
  convertEmojis = FALSE, 
  removeUrls = FALSE, 
  removeTwitterEntities = FALSE,
  removePunctuation = FALSE, 
  removeNumbers = FALSE, 
  removeRepeatedCharN = NULL, 
  removeAccents = FALSE, 
  stops = NULL, 
  tokenizeWords = FALSE, 
  stemLang = NULL,
  # Vocabulary pruning
  term_count_min = 1,
  term_count_max = Inf,
  doc_proportion_min = 0,
  doc_proportion_max = 1,
  max_number_of_terms = Inf,
  # Vectorization
  ngram_min = 1,
  ngram_max = 1,
  dtm_transformer = tf_transformer, # tf_transformer | tfidf_transformer | binary_transformer | identity
  feature_hashing = FALSE
)

# Preprocessing without using tm's corpus, i.e. on simple list of texts. 
# Requires control list c.
# --------------------------------------------------------------------------------
preprocess_text = function(text, c) {
  # Before tokenization
  if (c$toLower) text = tolower(text)
  if (c$convertEmojis) text = substitute_emoticons(text)
  if (c$removeUrls) text = remove_twitter_urls(text)
  if (c$removeTwitterEntities) text = remove_twitter_entities(text)
  # tm doesn't replace punct with space. Which means some words separate by period (e.g. "end.New) will be fused
  # into a single token later ("endNew")
  if (c$removePunctuation) text = gsub("[[:punct:]]+", " ", text) 
  if (c$removeNumbers) text = removeNumbers(text)
  if (!is.null(c$removeRepeatedCharN)) text = remove_repeated_char(text, c$removeRepeatedCharN)
  if (c$removeAccents) text = remove_accents(text)
  if (!is.null(c$stops)) text = removeWords(text, c$stops)
  if (c$tokenizeWords) text = word_tokenizer(text)
  # Tokenization returns list of character vectors. SnowballC's wordStem() accepts only vectors. => lapply 
  if (c$tokenizeWords && !is.null(c$stemLang)) text = lapply(text, function(x) wordStem(x, language=c$stemLang))
  text
}

# Construct iterator with custom preprocessor
# --------------------------------------------------------------------------------
get_t2v_iterator = function(text, ppc, progressbar=F) {
  itoken(text, 
         preprocess_function = function(x) preprocess_text(x, ppc), 
         tokenizer = ifelse(ppc$tokenizeWords, identity, word_tokenizer), 
         chunks_number=10, progessbar=progressbar)
}


# --------------------------------------------------------------------------------
get_t2v_corpus = function(text, ppc, vocab=NULL, verbose=F) {
  if (ppc$feature_hashing) {
    fh = feature_hasher(hash_size=2**18, ngram=c(ngram_min = ppc$ngram_min, ngram_max = ppc$ngram_max))
    corpus = create_hash_corpus(get_t2v_iterator(text, ppc), feature_hasher=fh)
  } else {
    if (is.null(vocab)) {
      if (verbose) { cat("\nCreating vocabulary...\n"); flush.console(); }
      vocab = vocabulary(get_t2v_iterator(text, ppc, verbose), ngram=c(ngram_min = ppc$ngram_min, ngram_max = ppc$ngram_max))
      vocab = prune_vocabulary(vocab, 
                               term_count_min = ppc$term_count_min,
                               term_count_max = ppc$term_count_max,
                               doc_proportion_min = ppc$doc_proportion_min,
                               doc_proportion_max = ppc$doc_proportion_max)
    }
    if (verbose) { print("\nCreating corpus...\n"); flush.console(); }
    corpus = create_vocab_corpus(get_t2v_iterator(text, ppc, verbose), vocabulary=vocab)
  }
  list(corpus=corpus, vocab=vocab)
}


# ---------------------------------------------------------------------------------------
get_t2v_dtm = function(corp, ppc) {
  corp %>% get_dtm %>% fix_dimnames %>% (ppc$dtm_transformer)
}


# Other text2vec helpers
# ---------------------------------------------------------------------------------------
fix_dimnames = function(dtm) {
  names(dtm@Dimnames) = c("Docs", "Terms")
  dtm@Dimnames$Docs = as.character(1:nrow(dtm))
  dtm
}

vocab_info = function(v, n=10, decr=T) {
  cat(sprintf("Have %i terms in total.\n\n", length(v$vocab$terms)))
  cat("Term count stats: \n\n")
  print(summary(v$vocab$terms_counts)); cat("\n\n")
  cat("Doc. propr. stats: \n\n")
  print(summary(v$vocab$doc_proportions)); cat("\n\n")
  
  ids = head(order(vocab$vocab$terms_counts, decreasing=decr), n)
  data.frame(term=v$vocab$terms[ids],
             count=v$vocab$terms_counts[ids],
             doc_prop=v$vocab$doc_proportions[ids],
             idx=ids
  )
}


# Remove empty docs in doc-term matrix and return modified corpus
# --------------------------------------------------------------------------------
which_empty_docs = function(corp, tm_ctrl) {
  dtm = DocumentTermMatrix(corp, control=tm_ctrl)
  rowtotal = apply(dtm, 1, sum)
  empty_rows = dtm[rowtotal == 0, ]$dimnames$Docs
  as.numeric(empty_rows)
}


# Load TASS XML tweets as df and preprocesses sentiments
# --------------------------------------------------------------------------------
load_tass_tweets = function(fnms, remove_non_sents=T, remove_non_es=T, remove_neu_sents=F, shuffle=T) {
  df = data.frame()
  for (fnm in fnms) {
    xml = xmlParse(fnm)
    df_tmp = data.frame(id = sapply(xml["//tweetid"], xmlValue), 
                    sentiment = sapply(xml["/tweets/tweet/sentiments/polarity[1]/value"], xmlValue),
                    text = sapply(xml["//content"], xmlValue),
                    lang = sapply(xml["//lang"], xmlValue))
    df = rbind(df, df_tmp)
  }
  
  if (shuffle) df = df[sample(nrow(df)),]
  
  df$text = as.character(df$text)
  df$id = as.double(as.character(df$id))
  
  # Take only non-None sentiments and only lang=es...
  if (remove_non_es) df = df[df$lang == "es", ]
  if (remove_non_sents) {
    df = df[df$sentiment != "NONE", ]
    df = droplevels(df)
  }
  if (remove_neu_sents) {
    df = df[df$sentiment != "NEU", ]
    df = droplevels(df)
  }
  
  # Add numeric polarity
  df$sent_num = revalue(as.character(df$sentiment), c("N+"=-2, "N"=-1, "NEU"=0, "P"=1, "P+"=2))
  df$sent_num = as.numeric(df$sent_num)
  df$sent_bin[df$sent_num < 0] = "Neg"
  df$sent_bin[df$sent_num == 0] = "Neu"
  df$sent_bin[df$sent_num > 0] = "Pos"
  df$sent_bin = as.factor(df$sent_bin)
  df
}


# Term-Document Matrix
# --------------------------------------------------------------------------------
tdm_control = function(ngrams=1, weighting="tfidf") {
  
  if (ngrams > 1) {
    tokenizer = ngrams_tokenizer(ngrams, package="NLP")  
  } else {
    tokenizer = "words"
  }
  
  if (weighting == "tfidf") {
    weight_func = function(x) weightTfIdf(x, normalize=T)
  } else if (weighting == "tf") {
    weight_func = function(x) weightTf(x)
  } else {
    weight_func = function(x) weightBin(x)
  }
  
  list(tokenize = tokenizer, weighting = weight_func)
}
