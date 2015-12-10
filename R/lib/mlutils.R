library(Matrix)

# Predict and print performance measure for binary classification
# --------------------------------------------------------------------------------
predict_perf_bin = function(model, newdata, newlabels, ...) {
  pred = predict(model, newdata, ...)
  if (class(pred) == 'matrix'){
    # Usually 1st column ?
    pred = factor(pred[,1])
  }
  print(confusionMatrix(pred, newlabels))
  pred
}

# Predict and print performance measure for multiclass classification
# --------------------------------------------------------------------------------
predict_perf_multi = function(model, newdata, newlabels, ...) {
  pred = predict(model, newdata, ...)
  df = data.frame(obs=newlabels, pred=pred)
  print(multiClassSummary(predf, lev=levels(y_te)))
  print(confusionMatrix(pred, newlabels))
  pred
}


# Creates smaller dataset (sparse matrix) in which each class has the same number of samples as the 
# originally smallest class. Simple sampling without replacement.
# --------------------------------------------------------------------------------
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

# Creates larger dataset (sparse matrix) in which each class has the same number of samples as the
# originally largest class. Samples with replacement,
# --------------------------------------------------------------------------------
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