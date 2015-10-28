# --------------------------------------------------------------------------------
# rm(list=ls())
# --------------------------------------------------------------------------------
source('common.R')

# Setup parameters
community_algo = "louvain"
day = "20150916"
mode = "R"
G = process_ggraph(mode=mode)
g = day_graph(day, mode=mode, com_algo=community_algo, G=G)


# Stats
# --------------------------------------------------------------------
is.connected(g)
component_sizes(g, "weak")
kcore_dist(g)

# Filter graph
g = filter_attr_not_in(g, attr="aff", c("animalistas", "unknown"))

vcount(g)
ecount(g)
group_sizes(g, "aff")
plot_group_sizes(g, grp_by="aff") + scale_color_manual(values=party_colors)

ia2df(important_actors_by_com(g, topn=10, centrality='indeg', grp_by="aff"))

im = round(interaction_matrix(g, grp_by="aff", scale=T), 2)
plot_interaction_matrix(im) + xlab("retweeted") + ylab("retweeter")


# Inspect instance and global communities
# --------------------------------------------------------------------
# Overlap of global and instance communities
globcoms_for_com = function(graph, comid) {
  sort(table(V(graph)[loc_com==comid]$com)/length(V(graph)[loc_com==comid]), decr=T)
}

globcom_for_com = function(graph, comid) {
  names(globcoms_for_com(graph, comid))[1]
}


coms_map = data.frame(com=names(group_sizes(g, "loc_com")))
coms_map$global_com = sapply(coms_map$com, function(x) globcom_for_com(g, x))
head(coms_map, 10)

# Nodes and degrees that don't agree in instance and global affiliation
unmatched = V(g)[loc_aff != aff]
top_unmatched = head(sort(degree(g, unmatched), decr=T), 20)
vertex_attribs(g, names(top_unmatched))

# Pairwise count of non-matching affiliations
all_unmatched = vertex_attribs(g, unmatched)
table(all_unmatched$aff, all_unmatched$loc_aff)

# Unknown affiliations
unknown = V(g)[loc_aff == "unknown"]
top_unknown = head(sort(degree(g, unknown), decr=T), 20)
vertex_attribs(g, names(top_unknown))

glob_unknown = V(g)[aff == "unknown"]
top_glob_unknown = head(sort(degree(g, glob_unknown), decr=T), 20)
vertex_attribs(g, names(top_glob_unknown))

both_unknown = V(g)[aff == "unknown" & loc_aff == "unknown"]
top_both_unknown = head(sort(degree(h, both_unknown), decr=T), 20)
vertex_attribs(h, names(top_both_unknown))


# Identify community "leaders"
# --------------------------------------------------------------------
imp_acts = ia2df(important_actors_by_com(g, topn=2, centrality='indeg', grp_by="aff"))
ia_flat = unlist(imp_acts, use.names=F)
incl_verts = which(V(g)$name %in% ia_flat)
V(g)$label = NA
V(g)[incl_verts]$label = V(g)[incl_verts]$name

cols = adjust_hsv(party_colors, "s", 0.6)
cols = adjust_hsv(cols, "v", 1.4)
V(g)$color = as.character(cols[V(g)$aff])
g_plot(g, layout=with_fr(), colorAttr=NULL, mode='r', emph_ids=incl_verts)

