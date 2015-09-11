rm(list=ls())

library(igraph)

# Setup directories
root_dir = "~/Code/tweetonomy/"
tblnm = "parties"
data_dir = paste0(root_dir, "data/", tblnm, "/")

setwd(paste0(root_dir, "R"))
source('utils.R')
source('plotutils.R')
source('fileutils.R')

# Setup parameters
day = "20150809"
community_algo = "fast_greedy"

args = commandArgs(trailingOnly=TRUE)
if (length(args) > 0) {
  day = args[1]
}

# Load or cache global communities
com_cache_fnm = paste0(data_dir, "r-cache/global-communities.Rdata")
total_edgelist_fnm = paste0(data_dir, "retweet-edges/all-retweet-edges.txt")
global_coms = global_communities(com_cache_fnm, total_edgelist_fnm, community_algo)

# Graph from edge lists
fnm = paste0(data_dir, "retweet-edges/", day, ".txt")
g = load_graph(fnm)

# Communities
g$communities = find_communities(g, community_algo)
V(g)$com = membership(g$communities)
V(g)$global_com = membership(global_coms)[V(g)$name]


# Filter graph
# --------------------------------------------------------------------
h = g %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_topn_components(topn=1, mode="weak") %>%
    filter_topn_communities(n=10, algo=community_algo, comname="com") %>%
    filter_isolates()


# Match groups to affiliation
# --------------------------------------------------------------------
h = set_affiliations(h, grp_by="com", aff_attr="aff", party_affiliations)
h = set_affiliations(h, grp_by="global_com", aff_attr="global_aff", party_affiliations)


# Content analysis (hashtags)
# --------------------------------------------------------------------
nodedf = igraph:::as_data_frame(h, what="vertices")
tags_fnm = paste0(data_dir, "hashtags-by-user/", day, ".txt")
usertags = hashtags(tags_fnm)

# Not all users will appear in usertags, as some users will have tweeted always without 
# hashtags! Equally, not all users in usertags will appear in retweet graph, as
# not all tweets are retweets. Hence we need to use inner-join
num_top_tags = 100
innerdf = inner_join(nodedf, usertags, by='name')

# Tag count by community
tagcount_by_grp = function(grp_by, topn=100) {
  sapply(names(group_sizes(h, grp_by)), function(x) {
    as.table(head(sort(table(unlist(innerdf[innerdf[grp_by]==x, "hashtags"])), decr=T), topn))
  }, simplify=F)
}

# Store daily hashtag frequency
tagfreqs_fnm = paste0(data_dir, "r-cache/tagfreqs.Rdata")
tagfreqs = get_cached("tagfreqs", tagfreqs_fnm)
if (is.null(tagfreqs)) {
  tagfreqs = list()
}
tagfreqs[[day]] = tagcount_by_grp("global_aff")
save(tagfreqs, file=tagfreqs_fnm)
