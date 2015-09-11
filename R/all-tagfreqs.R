rm(list=ls())

library(igraph)
library(tools)

# Setup directories
root_dir = "~/Code/tweetonomy/"
tblnm = "parties"
data_dir = paste0(root_dir, "data/", tblnm, "/")

setwd(paste0(root_dir, "R"))
source('utils.R')
source('fileutils.R')

# Setup parameters
community_algo = "fast_greedy"
topn_coms = 10

# Load or cache global communities
com_cache_fnm = paste0(data_dir, "r-cache/global-communities.Rdata")
total_edgelist_fnm = paste0(data_dir, "retweet-edges/all-retweet-edges.txt")
tagfreqs_fnm = paste0(data_dir, "r-cache/tagfreqs.Rdata")
global_coms = global_communities(com_cache_fnm, total_edgelist_fnm, community_algo)

# Loop over all days
# --------------------------------------------------------------------------------
cache_all_tagfreqs = function() {
  day_files = file_path_sans_ext(list.files(paste0(data_dir, "retweet-edges/")))
  day_files = day_files[!is.na(as.numeric(day_files))]
  
  for (day in day_files) {
    print(paste0("Processing day ", day))
    globalcom_tags_for_day(day, tagfreqs_fnm)
  }
}


# Tag count by community
# --------------------------------------------------------------------------------
tagcount_by_grp = function(g, tagdf, grp_by, topn=100) {
  sapply(names(group_sizes(g, grp_by)), function(x) {
    as.table(head(sort(table(unlist(tagdf[tagdf[grp_by]==x, "hashtags"])), decr=T), topn))
  }, simplify=F)
}


# Return tag frequencies per community for given day
# --------------------------------------------------------------------------------
globalcom_tags_for_day = function(daystr, tagfs_fnm) {
  
  # Graph from edge lists
  fnm = paste0(data_dir, "retweet-edges/", daystr, ".txt")
  G = load_graph(fnm)

  # Assign global communities
  V(G)$global_com = membership(global_coms)[V(G)$name]
  topn_com_names = names(head(sort(group_sizes(G, "global_com"), decr=T), topn_coms))  

  # Filter graph, keeping only topn communities
  G = G %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_topn_components(topn=1, mode="weak") %>%
    filter_attr_in("global_com", topn_com_names) %>%
    filter_isolates()

  # Match groups to affiliation
  G = set_affiliations(G, grp_by="global_com", aff_attr="global_aff", party_affiliations)

  # Content analysis (hashtags)
  nodedf = igraph:::as_data_frame(G, what="vertices")
  tags_fnm = paste0(data_dir, "hashtags-by-user/", daystr, ".txt")
  usertags = hashtags(tags_fnm)

  # Not all users will appear in usertags, as some users will have tweeted always without 
  # hashtags! Equally, not all users in usertags will appear in retweet graph, as
  # not all tweets are retweets. Hence we need to use inner-join
  num_top_tags = 100
  innerdf = inner_join(nodedf, usertags, by='name')

  # Store daily hashtag frequency  
  tagfreqs = get_cached("tagfreqs", tagfs_fnm)
  if (is.null(tagfreqs)) {
    tagfreqs = list()
  }
  daycount = tagcount_by_grp(G, innerdf, "global_aff")
  tagfreqs[[daystr]] = daycount
  save(tagfreqs, file=tagfs_fnm)
  return(daycount)
}

# "Main"
# --------------------------------------------------------------------------------
cache_all_tagfreqs()
