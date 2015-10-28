# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')
library(plyr)
library(stringr)
library(stringi)



day = "20150804"
tweets_fnm = paste0(data_dir, "tweets/tweets-", day,  ".tsv")

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

t = read.table(tweets_fnm, sep='\t', quote="", comment.char="", header=F, col.names=c("id", "user", "text"), 
               colClasses=c("character", "character", "character"))

pos = scan(pos_fnm, what="character")
neg = scan(neg_fnm, what="character")


# http://www.slideshare.net/jeffreybreen/r-by-example-mining-twitter-for
clean_words = function(txt) {
  txt = gsub('[[:punct:]]', "", txt)
  txt = gsub('[[:cntrl:]]', "", txt)
  txt = gsub('\\d+', "", txt)
  txt = tolower(txt)  
  words = unlist(str_split(txt, '\\s+'))
  words = stri_trans_general(words, "Latin-ASCII")
  words[words != ""]
}

matching_words = function(txt, vocab) {
  words = clean_words(txt)
  matches = match(words, vocab)
  words[which(!is.na(matches))]
}

num_matching = function(txt, vocab) {
  words = clean_words(txt)
  sum(!is.na(match(words, vocab)))
}

sentiment = function(txt, pos, neg) {
  words = clean_words(txt)
  pos_matches = !is.na(match(words, pos))
  neg_matches = !is.na(match(words, neg))
  sum(pos_matches) - sum(neg_matches)
}

sentiments = function(txts, pos, neg, parallel=F, prog="text") {
  laply(txts, function(x) sentiment(x, pos, neg), .parallel=parallel, .progress=prog)
}

s = sentiments(t$text, pos, neg)

w = which(s < -3)
t$text[w]
