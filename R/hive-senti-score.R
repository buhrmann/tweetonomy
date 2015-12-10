#!/Library/Frameworks/R.framework/Resources/bin/Rscript
#
# ---------------------------------------------------------------------------------------
# Wrapper for a scoring model to be used by Hive
# Based on slides "Large-scale predictive modelling..." by Alez Zolotovitski.
# ---------------------------------------------------------------------------------------
library(Matrix, quietly=TRUE)
library(glmnet, quietly=TRUE)
source("/Users/thomasbuhrmann/Code/tweetonomy/R/lib/textutils.R", chdir=TRUE, echo=FALSE, verbose=FALSE)

# ---------------------------------------------------------------------------------------
# This will be executed on hdfs where the data is provided along with this script
# I.e. path specified in command line arg 1 is not for local filesystem
# The below loads the model, ppc, and vocab
args = commandArgs(trailingOnly = TRUE)
scorer = load(args[1])

batch.size = 30000
f = file('stdin', 'r')

while (TRUE) {
  # Get batch.size worth of data from stdin or stop if data exhausted
  x = read.delim(f, header=F, sep='\t', stringsAsFactors=F, col.names=c("id", "text"), nrows=batch.size)
  if (nrow(x) == 0) {
    break
  }
  
  # Preprocess and transform raw text using same parameters and vocabulary as trained model
  corp = get_t2v_corpus(x$text, ppc, vocab, verbose=F)
  dtm = get_t2v_dtm(corp$corpus, ppc)
  
  # Create and return predictions
  y = predict(mod_glm, dtm, type="class")
  if (class(y) == 'matrix') {
    y = factor(y[,1])
  }
  # Store in boolean format to save space in hive table
  y = ifelse(y == "Pos", 1, 0)
  tb = data.frame(id=x$id, positive=y)
  #write.table(tb, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", file=stderr())
  write.table(tb, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t")
}
