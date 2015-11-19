# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')

# Setup parameters
tagfreqs_fnm = list(R=paste0(data_dir, "r-cache/retweet-tagfreqs.Rdata"),
                    M=paste0(data_dir, "r-cache/mention-tagfreqs.Rdata"))


# Loop over all days
# Needs version because communities are identified from global graph, which itself 
# comes in versions
# --------------------------------------------------------------------------------
cache_all_tagfreqs = function(layer, version, G, from="00000000", to="99999999") {
  fld = ifelse(layer=="R", "retweet-edges", "mention-edges")
  day_files = file_path_sans_ext(list.files(paste0(data_dir, fld)))
  day_files = day_files[!is.na(as.numeric(day_files))]
  day_files = day_files[(day_files >= from) & (day_files <= to)]
  
  for (day in day_files) {
    print(sprintf("Processing day %s", day)); flush.console()
    globalcom_tags_for_day(day, layer, version, tagfreqs_fnm[[layer]], G)
  }
  get_cached("tagfreqs", tagfreqs_fnm[[layer]])
}


# Tag count by community
# --------------------------------------------------------------------------------
tagcount_by_grp = function(tagdf, grp_by, topn=100) {
  grp_names = sort(table(tagdf[grp_by]), decr=T)
  sapply(names(grp_names), function(x) {
    as.table(head(sort(table(unlist(tagdf[tagdf[grp_by]==x, "hashtags"])), decr=T), topn))
  }, simplify=F)
}


# Return tag frequencies per community for given day
# If from_graph is true, builds day graph and extracts users from there. If false,
# uses day_users instead which should give same result but faster.
# --------------------------------------------------------------------------------
globalcom_tags_for_day = function(daystr, layer, version, tagfs_fnm, G, from_graph=F) {
  
  # Graph from edge lists
  if (from_graph) {
    g = day_graph(daystr, layer=layer, version-version, com_algo=community_algo, G=G)    
    nodedf = igraph::as_data_frame(g, what="vertices")
  } else {
    nodedf = day_users_with_com(daystr, layer=layer, version=version, G=G)    
  }
  
  if (is.null(nodedf) || nrow(nodedf) == 0) { 
    print("No information for this day. Returning NULL."); flush.console()
    return(NULL) 
  }
  
  nodedf = filter(nodedf, !(aff %in% c("animalistas", "unknown")))
  
  # Content analysis (hashtags)
  tags_fnm = paste0(data_dir, "hashtags-by-user/", daystr, ".txt")
  usertags = hashtags(tags_fnm)
  
  # Not all users will appear in usertags, as some users will have tweeted always without 
  # hashtags! Equally, not all users in usertags will appear in retweet graph, as
  # not all tweets are retweets. Hence we need to use inner-join
  innerdf = inner_join(nodedf, usertags, by='name')

  # Store daily hashtag frequency  
  tagfreqs = get_cached("tagfreqs", tagfs_fnm)
  if (is.null(tagfreqs)) {
    tagfreqs = list()
  }
  daycount = tagcount_by_grp(innerdf, "aff")
  tagfreqs[[daystr]] = daycount
  save(tagfreqs, file=tagfs_fnm)
  daycount
}


# Collect all tag frequencies into long data frame
# ---------------------------------------------------------------------------
tag_tbl_df = function(tagfreqs, daystr, affstr) {
  tbl = tagfreqs[[daystr]][[affstr]]
  df = as.data.frame(tbl)
  names(df)[names(df) == 'Var1'] = 'tag'
  df$aff = affstr
  df$day = as.Date(daystr, "%Y%m%d")
  return(df)
}

tag_day_df = function(tagfreqs, daystr) {
  affs = names(tagfreqs[[daystr]])
  do.call(rbind, lapply(affs, function(x) tag_tbl_df(tagfreqs, daystr, x)))
}

tag_all_df = function(tagfreqs) {
  days = names(tagfreqs)
  do.call(rbind, lapply(days, function(day) tag_day_df(tagfreqs, day)))
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

# Plot functions
# --------------------------------------------------------------------------------
plot_tagfreq = function(tags_df, tagval) {
  d = filter(tags_df, tag==tagval)
  ggplot(d, aes(x=day, y=Freq, colour=aff)) + 
    geom_line() + theme_minimal() + ggtitle(tagval) +
    scale_color_manual(values=unlist(party_colors))
}

# "Main"
# --------------------------------------------------------------------------------
#freqs = cache_all_tagfreqs(mode=mode, tagfreqs_fnm)
#freqs = get_cached("tagfreqs", tagfreqs_fnm)

# common_tags = tag_intersect_day(10)
# ctag = names(common_tags[2])
# 
# tags_df = tag_all_df()
# tags_df %>%
#   group_by(tag) %>%
#   summarise(freq = sum(Freq)) %>%
#   arrange(desc(freq)) %>%
#   print(n=50)
# 
# plot_tagfreq("pge2016") + scale_color_manual(values=unlist(party_colors))


