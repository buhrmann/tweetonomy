# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')

tagfreqs_fnm = paste0(data_dir, "r-cache/tagfreqs.Rdata")
tagfreqs = get_cached("tagfreqs", tagfreqs_fnm)

# Collect all tag frequencies into long data frame
# ---------------------------------------------------------------------------
tag_tbl_df = function(daystr, affstr) {
  tbl = tagfreqs[[daystr]][[affstr]]
  df = as.data.frame(tbl)
  names(df)[names(df) == 'Var1'] = 'tag'
  df$aff = affstr
  df$day = as.Date(daystr, "%Y%m%d")
  return(df)
}

tag_day_df = function(daystr) {
  affs = names(tagfreqs[[daystr]])
  do.call(rbind, lapply(affs, function(x) tag_tbl_df(daystr, x)))
}

tag_all_df = function() {
  days = names(tagfreqs)
  do.call(rbind, lapply(days, tag_day_df))
}


# Number of communities using the same hashtag
# ---------------------------------------------------------------------------
tag_intersect_aff = function(day, topn) {
  head(sort(table(unlist(lapply(tagfreqs[[day]], names), use.names=F)), decr=T), topn)
}

# "Number of days" a common tag has been used
tag_intersect_day = function(topn) {
  days = names(tagfreqs)
  inters_aff = lapply(days, function(x) tag_intersect_aff(x, topn))
  sort(table(unlist(lapply(inters_aff, names))), decr=T)
}

# Explore
# ---------------------------------------------------------------------------
common_tags = tag_intersect_day(10)
ctag = names(common_tags[2])

tags_df = tag_all_df()
tags_df %>%
  group_by(tag) %>%
  summarise(freq = sum(Freq)) %>%
  arrange(desc(freq)) %>%
  print(n=50)

plot_tagfreq = function(tagval) {
  d = filter(tags_df, tag==tagval)
  #print(d)
  ggplot(d, aes(x=day, y=Freq, colour=aff)) + 
    geom_line() + theme_minimal() +
    ggtitle(tagval)
}

plot_tagfreq("pge2016") + scale_color_manual(values=unlist(party_colors))


