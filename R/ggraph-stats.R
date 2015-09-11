
source('common.R')

community_algo = "louvain"
topn_coms = 10
ggraph_fnm = paste0(data_dir, "r-cache/global-graph.Rdata")
total_edgelist_fnm = paste0(data_dir, "retweet-edges/all-retweet-edges.txt")

GG = global_graph(ggraph_fnm, total_edgelist_fnm, community_algo)
GG = GG %>% 
  simplify(edge.attr.comb=list(weight="sum")) %>%
  filter_topn_components(topn=1, mode="weak")

# Basic stats
# --------------------------------------------------------------------------------
vcount(GG)
ecount(GG)
component_sizes(GG, "weak")
component_sizes(GG, "strong")
kcore_dist(GG)

# Weight distribution
weight_dist(GG)
barplot(weight_dist(GG), log="xy")
ggplot(plyr::count(E(GG)$weight), aes(x=x, y=freq)) + geom_point(stat='identity') + scale_y_log10() + scale_x_log10() + theme_minimal()

# Degree distribution
barplot(table(degree(GG)), log="xy")
ggplot(plyr::count(degree(GG)), aes(x=x, y=freq)) + geom_point(stat='identity') + scale_y_log10() + scale_x_log10() + theme_minimal()

# Community size distribution
group_sizes(GG, "com")
plot(group_sizes(GG, "com"), log="xy", pch=20, xlab="group index", ylab="size")

# Compare algos
GG$communities_lv = find_communities(GG, "louvain")
V(GG)$com_lv = membership(GG$communities_lv)
V(GG)$affl = affiliations_list(membership(GG$communities))
V(GG)$affl_lv = affiliations_list(membership(GG$communities_lv))
unmatched = which(V(GG)$affl != V(GG)$affl_lv)
top_unmatched = head(sort(degree(GG, unmatched), decr=T), 20)
vertex_attribs(GG, names(top_unmatched))
vertex_attribs(GG, names(head(sort(degree(GG, which(V(GG)$affl=='unknown')), decr=T), 10)) )
vertex_attribs(GG, names(head(sort(degree(GG, which(V(GG)$affl_lv=='unknown')), decr=T), 10)) )
# Pairwise count of non-matching affiliations
all_unmatched = vertex_attribs(GG, unmatched)
table(all_unmatched$affl, all_unmatched$affl_lv)


# Identify important actors
important_actors_to_df(important_actors_by_com(GG, topn=10, centrality="pgrank", grp_by="affl"))
important_actors_to_df(important_actors_by_com(GG, topn=10, centrality="pgrank", grp_by="affl_lv"))
save(GG, file=ggraph_fnm)

# Filter
# --------------------------------------------------------------------------------
FF = GG %>% 
  simplify(edge.attr.comb=list(weight="sum")) %>%
  filter_topn_components(topn=1, mode="weak") %>%
  filter_topn_communities(n=10, algo=community_algo, comname="com") %>%
  #filter_min_community_size(min_size=400) %>%
  filter_isolates()

FF = set_affiliations(FF, grp_by="com", aff_attr="aff", party_affiliations)

# Unknown affiliations
unknown = V(FF)[aff == "unknown"]
top_unknown = head(sort(degree(FF, unknown), decr=T), 20)
vertex_attribs(FF, names(top_unknown))
unique(vertex_attr(FF, "com", unknown))

group_acts = important_actors_to_df(important_actors_by_com(FF, topn=10, centrality="indeg", grp_by="aff"))
#group_acts = important_actors_overall(h, topn_per=10, topn=2)
print(group_acts)

