rm(list=ls())

library(igraph)
library(networkD3)
library(tcltk)
library(rgl)
library(dplyr)

setwd("~/Code/tweetonomy/R")
source('utils.R')


# Graph from edge lists
colnames = c("from", "to", "weight")
fnm = "~/Code/tweetonomy/data/retweets-parties-20150804.txt"
edge_list = read.table(fnm, sep='\t', header=F, col.names=colnames)
g = graph.data.frame(edge_list)

# Stats
# --------------------------------------------------------------------
is.connected(g)
table(sapply(decompose(g), vcount))
component_sizes(g, "weak")
kcore_dist(g)
vcount(g)
ecount(g)

# communities
coms = find_communities(g, 'fast_greedy')
head(sort(sizes(coms), decreasing=T), 25)

# dplyr version
# --------------------------------------------------------------------
community_algo = "fast_greedy"
# h = g %>% 
#   simplify(edge.attr.comb=list(weight="sum")) %>%
#   filter_small_components(min_size=3, mode="weak") %>%
#   filter_min_coreness(min_coreness=2) %>%
#   filter_topn_communities(n=10, algo=community_algo) %>%
#   filter_isolates() %>%
#   g_plot(layout=NULL, colorAttr=NULL, mode='r')

h = g %>% 
  simplify(edge.attr.comb=list(weight="sum")) %>%
  filter_topn_components(topn=1, mode="weak") %>%
  filter_topn_communities(n=10, algo=community_algo) %>%
  filter_isolates() %>%
  g_plot(layout=NULL, colorAttr=NULL, mode='r')

h = g_plot(h, layout=with_drl(), colorAttr=NULL, mode='r')

# Highlight community "leaders"
cents = centralities(h)
group_acts = important_actors_by_com(h, topn=3, centrality="degree")
group_acts = c("PPopular", "ahorapodemos", "iunida", "CiudadanosCs", "PSOE")
incl_verts = which((V(h)$name %in% unlist(group_acts)))
V(h)$label = NA
V(h)[incl_verts]$label = V(h)[incl_verts]$name
g_plot(h, layout=with_drl(), colorAttr=NULL, mode='r', emph_ids=incl_verts)


# Join vertices with hashtag count
nodedf = igraph:::as_data_frame(h, what="vertices")
tag_list = function(str) {
  return (strsplit(gsub("[\\[\\]]", "", str, perl=T), ','))
}
usertags = read.table("~/Code/tweetonomy/data/hashtags-by-user-20150804.txt", sep='\t', header=F, col.names=c("name", "hashtags"))
usertags$tags = tag_list(usertags$hashtags)
usertags$num = sapply(usertags$tags, length)
usertags$hashtags = NULL

# Remove most common tags (e.g. "greece")
n = 7
tagfreq = sort(table(unlist(usertags$tags)), decr=T)
common_tags = names(tagfreq[1:n])
usertags$tagsred = sapply(usertags$tags, FUN=function(x) { 
  y = unlist(x);
  return(y[!(y %in% common_tags)]) })
  #return(x[x != "greece"]) })

# Not all users will appear in user_tags, as some users will 
# have tweeted always without hashtags!
num_top_tags = 5
innerdf = inner_join(nodedf, usertags, by='name')
comtags = innerdf %>% 
  group_by(com) %>%
  summarise(top=list(head((sort(table(unlist(tags)), decr=T)), num_top_tags)))

comtags$top
