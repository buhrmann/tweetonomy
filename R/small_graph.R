rm(list=ls())

library(igraph)
library(networkD3)
library(tcltk)
library(rgl)
library(dplyr)

# Setup directories
root_dir = "~/Code/tweetonomy/"
tblnm = "parties"
data_dir = paste0(root_dir, "data/", tblnm, "/")

setwd(paste0(root_dir, "R"))
source('utils.R')
source('plotutils.R')
source('fileutils.R')

# Setup parameters
day = "20150812"
community_algo = "fast_greedy"


# Load or cache global communities
globcoms_fnm = paste0(data_dir, "global-communities.Rdata")
global_coms = get_cached("global_coms", globcoms_fnm)
if (is.null(global_coms)) {
  print("Generating global coms file.")
  fnm = paste0(data_dir, "retweet-edges/all-retweet-edges.txt")
  colnames = c("from", "to", "weight")
  edge_list = read.table(fnm, sep='\t', header=F, col.names=colnames)
  glob_g = graph.data.frame(edge_list)
  global_coms = find_communities(glob_g, community_algo)
  save(global_coms, file=globcoms_fnm)
  rm(glob_g)
}


# Graph from edge lists
fnm = paste0(data_dir, "retweet-edges/", day, ".txt")
colnames = c("from", "to", "weight")
edge_list = read.table(fnm, sep='\t', header=F, col.names=colnames)
g = graph.data.frame(edge_list)


# Stats
# --------------------------------------------------------------------
is.connected(g)
#table(sapply(decompose(g), vcount)) following line is the same but faster
component_sizes(g, "weak")
kcore_dist(g)
vcount(g)
ecount(g)

# communities
g$communities = find_communities(g, community_algo)
V(g)$com = membership(g$communities)
V(g)$global_com = membership(global_coms)[V(g)$name]
head(sort(sizes(g$communities), decreasing=T), 25)


# Filter graph
# --------------------------------------------------------------------
lout = NULL
lout = with_drl()
reducer = 2
if (reducer == 1) {
  h = g %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_min_degree(min_deg=3) %>%
    filter_small_components(min_size=3, mode="weak") %>%
    filter_min_coreness(min_coreness=2) %>%
    filter_topn_communities(n=10, algo=community_algo) %>%
    #filter_min_community_size(min_size=10) %>%
    filter_isolates()
} else {
  h = g %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_topn_components(topn=1, mode="weak") %>%
    filter_topn_communities(n=10, algo=community_algo, comname="com") %>%
    #filter_min_community_size(min_size=400) %>%
    filter_isolates()
}

#h = g_plot(h, layout=lout, colorAttr="com", mode='r')
#h = g_plot(h, layout=lout, colorAttr="global_com", mode='r')

V(h)$indeg = as.numeric(centrality(h, "indeg"))

# Inspect instance and global communities
# --------------------------------------------------------------------
com_sizes = group_sizes(h, "com")
group_sizes(h, "global_com")

# Overlap of global and instance communities
globcoms_for_com = function(graph, comid) {
  sort(table(V(graph)[com==comid]$global_com)/length(V(graph)[com==comid]), decr=T)
}

globcom_for_com = function(graph, comid) {
  names(globcoms_for_com(graph, comid))[1]
}

sapply(names(com_sizes), function(x) head(globcoms_for_com(h, x))) 
coms_map = data.frame(com=names(com_sizes))
coms_map$global_com = sapply(coms_map$com, function(x) globcom_for_com(h, x))
coms_map

h = set_affiliations(h, grp_by="com", aff_attr="aff", party_affiliations)
h = set_affiliations(h, grp_by="global_com", aff_attr="global_aff", party_affiliations)

# Nodes and degrees that don't agree in instance and global affiliation
unmatched = V(h)[global_aff != aff]
top_unmatched = head(sort(degree(h, unmatched), decr=T), 20)
vertex_attribs(h, names(top_unmatched))

# Pairwise count of non-matching affiliations
all_unmatched = vertex_attribs(h, unmatched)
table(all_unmatched$aff, all_unmatched$global_aff)

# Unknown affiliations
unknown = V(h)[aff == "unknown"]
top_unknown = head(sort(degree(h, unknown), decr=T), 20)
vertex_attribs(h, names(top_unknown))

glob_unknown = V(h)[global_aff == "unknown"]
top_glob_unknown = head(sort(degree(h, glob_unknown), decr=T), 20)
vertex_attribs(h, names(top_glob_unknown))

both_unknown = V(h)[aff == "unknown" & global_aff == "unknown"]
top_both_unknown = head(sort(degree(h, both_unknown), decr=T), 20)
vertex_attribs(h, names(top_both_unknown))
#vertex_attribs(h, get_group(h, '7'))

# Identify community "leaders"
# --------------------------------------------------------------------
#cents = centralities(h)
group_acts = important_actors_to_df(important_actors_by_com(h, topn=10, centrality="indeg", grp_by="aff"))
#group_acts = important_actors_overall(h, topn_per=10, topn=2)
print(group_acts)

group_acts_flat = unlist(group_acts[1:2,], use.names=F)
#group_acts = c("PPopular", "ahorapodemos", "iunida", "CiudadanosCs", "PSOE")
incl_verts = which(V(h)$name %in% group_acts_flat)
V(h)$label = NA
V(h)[incl_verts]$label = V(h)[incl_verts]$name
g_plot(h, layout=with_drl(), colorAttr=NULL, mode='r', emph_ids=incl_verts)


# Content analysis (hashtags)
# --------------------------------------------------------------------
nodedf = igraph:::as_data_frame(h, what="vertices")

usertags = read.table(paste0(data_dir, "hashtags-by-user/", day, ".txt"), 
                      sep='\t', header=F, col.names=c("name", "hashtags"),
                      colClasses=c("character", "character"), encoding="UTF-8")

usertags$hashtags = tag_list(usertags$hashtags, to_ascii=T)
usertags$num = sapply(usertags$hashtags, length)
#usertags$tagsred = rm_topn(usertags$hashtags, 3)

# Most frequent hashtags overall
top_hashtags = head(sort(table(unlist(usertags$hashtags)), decr=T), 25)
top_hashtags

# Not all users will appear in usertags, as some users will have tweeted always without 
# hashtags! Equally, not all users in usertags will appear in retweet graph, as
# not all tweets are retweets. Hence we need to use inner-join
num_top_tags = 100
innerdf = inner_join(nodedf, usertags, by='name')

# Tag count by community
tagcount_by_grp = function(grp_by, topn=100) {
  sapply(names(group_sizes(h, grp_by)), function(x) {
    head(sort(table(unlist(innerdf[innerdf[grp_by]==x, "hashtags"])), decr=T), topn)
  }, simplify=F)
}


# Store daily hashtag frequency
tagfreqs_fnm = paste0(data_dir, "tagfreqs.Rdata")
tagfreqs = get_cached("tagfreqs", tagfreqs_fnm)
if (is.null(tagfreqs)) {
  tagfreqs = list()
}
tagfreqs[[day]] = tagcount_by_grp("aff")
save(tagfreqs, file=tagfreqs_fnm)
