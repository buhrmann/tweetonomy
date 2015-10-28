# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')

# Setup parameters
tagfreqs_fnm = paste0(data_dir, "r-cache/tagfreqs.Rdata")
community_algo = "louvain"
mode = "R"
G = process_ggraph(mode=mode)


# Loop over all days
# --------------------------------------------------------------------------------
cache_all_tagfreqs = function(mode, cache_fnm, from="00000000", to="99999999") {
  fld = ifelse(mode=="R", "retweet-edges", "mention-edges")
  day_files = file_path_sans_ext(list.files(paste0(data_dir, fld)))
  day_files = day_files[!is.na(as.numeric(day_files))]
  day_files = day_files[(day_files >= from) & (day_files <= to)]
  
  for (day in day_files) {
    print(sprintf("Processing day %s", day))
    flush.console()
    globalcom_tags_for_day(day, cache_fnm)
  }
  get_cached("tagfreqs", cache_fnm)
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
globalcom_tags_for_day = function(daystr, tagfs_fnm, from_graph=F) {
  
  # Graph from edge lists
  if (from_graph) {
    g = day_graph(daystr, mode=mode, com_algo=community_algo, G=G)    
    nodedf = igraph::as_data_frame(g, what="vertices")
  } else {
    nodedf = day_users_with_com(daystr, mode=mode, G=G)    
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

# "Main"
# --------------------------------------------------------------------------------
freqs = cache_all_tagfreqs(mode=mode, tagfreqs_fnm)
#freqs = get_cached("tagfreqs", tagfreqs_fnm)

