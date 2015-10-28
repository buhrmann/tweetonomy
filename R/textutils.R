library(XML)
library(plyr)
library(stringr)
library(stringi)

library(tm)
library(RTextTools)
library(RWeka)
library(topicmodels)
library(NLP)

# --------------------------------------------------------------------------------
ngrams_tokenizer = function(n, package="RWeka") {
  if (package == "RWeka") {
    options(mc.cores=1) # Still necessary with below explicit namesapce?
    tokenizer = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n))
  } else if (package == "NLP") {
    tokenizer = function(x) unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "), use.names = FALSE)
  }
  tokenizer
}


# text could be e.g. a data frame column
# --------------------------------------------------------------------------------
substitute_emoticons = function(text, pos_str="emopolpos", neg_str="emopolneg") {
  emo_pos = c(":\\)", ":-\\)", ";\\)", ";-\\)", " :D")
  emo_neg = c(":\\(", ":-\\(", ";-\\(", ";\\(")
  
  for (e in emo_pos) text = gsub(e, " emopolpos ", text)
  for (e in emo_neg) text = gsub(e, " emopolneg ", text)
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
preprocessed_corpus = function(text, stops=NULL, stemlang=NULL, remove_accents=T) {
  corp = Corpus(VectorSource(text))
  corp = tm_map(corp, content_transformer(tolower))
  corp = tm_map(corp, removePunctuation)
  corp = tm_map(corp, removeNumbers)
  
  corp = tm_map(corp, content_transformer(function(x) gsub("http[[:alnum:]]*", "", x)))
  
  if (!is.null(stops)) corp = tm_map(corp, removeWords, stops)
  if (!is.null(stemlang)) corp = tm_map(corp, stemDocument, language=stemlang)
  if (remove_accents) corp = tm_map(corp, content_transformer(function(x) stri_trans_general(x, "Latin-ASCII")))
  
  corp = tm_map(corp, stripWhitespace)
  corp
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
load_tass_tweets = function(fnm, remove_non_sents=T, remove_non_es=T) {
  xml = xmlParse(fnm)
  df = data.frame(id = sapply(xml["//tweetid"], xmlValue), 
                  sentiment = sapply(xml["/tweets/tweet/sentiments/polarity[1]/value"], xmlValue),
                  text = sapply(xml["//content"], xmlValue),
                  lang = sapply(xml["//lang"], xmlValue))
  
  df$text = as.character(df$text)
  df$id = as.double(as.character(df$id))
  
  # Take only non-None sentiments and only lang=es...
  if (remove_non_es) df = df[df$lang == "es", ]
  if (remove_non_sents) {
    df = df[df$sentiment != "NONE", ]
    df = droplevels(df)
  }
  
  # Add numeric polarity
  df$sent_num = revalue(as.character(df$sentiment), c("N+"=-2, "N"=-1, "NEU"=0, "P"=1, "P+"=2))
  df$sent_num = as.numeric(df$sent_num)
  df$sent_bin[df$sent_num < 0] = "Neg"
  df$sent_bin[df$sent_num == 0] = "Neu"
  df$sent_bin[df$sent_num > 0] = "Pos"
  df
}
