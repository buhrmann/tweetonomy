# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')

count_dir = "hourly-count/"
daycnt_cache = paste0(data_dir, "r-cache/tweetfreqs.Rdata")

fnm_for_day_tweets = function(day) { paste0(data_dir, count_dir, day, ".txt") }

# Loop over all days
# --------------------------------------------------------------------------------
all_day_counts = function(from="00000000", to="99999999", verbose=F) {
  day_files = file_path_sans_ext(list.files(paste0(data_dir, count_dir)))
  day_files = day_files[!is.na(as.numeric(day_files))]
  day_files = day_files[(day_files >= from) & (day_files <= to)]
  
  freqs = data.frame()
  for (day in day_files) {
    if (verbose) print(sprintf("Processing day %s", day));flush.console()
    if(file.info(fnm_for_day_tweets(day))$size == 0) {
      if (verbose) print("Skipping empty file"); flush.console()
      next
    }
    freqs = rbind(freqs, day_count(day))
  }
  freqs
}


# Data frame for single day tweet counts
# --------------------------------------------------------------------------------
day_count = function(day) {
  fnm = paste0(data_dir, count_dir, day, ".txt")
  counts = read.table(fnm, sep='\t', header=F, col.names=c("hour", "count"))
  # Not ordered by hour and some hours missing, hence:
  freqs = data.frame(datetime=0:23, freq=rep(0,24))
  for (i in 1:nrow(counts)) {
    freqs[counts$hour[i]+1, "freq"] = counts$count[i] 
  }
  datetime_str = paste0(day, " ", freqs$datetime)
  freqs$datetime = as.POSIXct(datetime_str, tz="", format="%Y%m%d %H")
  freqs
}


# --------------------------------------------------------------------------------
plot_tweet_freq = function(freqs, points=F) {
  p = ggplot(freqs, aes(x=datetime, y=freq)) + 
    geom_line() +
    theme_minimal() + xlab("") + ylab("#tweets") + 
    ggtitle(sprintf("Total #tweets: %s", pp_int(sum(freqs$freq))))
  if (points) {
    p = p + geom_point()
  }
  p
}
