source('common.R')
source('plotutils.R')
library(xtable)
library(boot)
library(reldist)

community_algo = "louvain"

process_ggraph = function() {
  G = global_graph(ggraph_fnm, total_edgelist_fnm, community_algo)
  
  G = G %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_min_weight(min_w=1) %>%
    filter_topn_components(topn=1, mode="weak") %>%
    filter_isolates()
}

g = process_ggraph()

plot_group_sizes(G, grp_by="com", log_scale=NULL, topn=11)


# Nodes in unknown affiliation
unknown = which(V(g)$aff=='unknown')
vertex_attribs(g, names(head(sort(degree(g, unknown), decr=T), 25)) )
# Communities most often categorized as unknown
head(sort(table(V(g)[unknown]$com), decr=T), 20)

# Important actors per community
centr = "degree"
acts = important_actors_by_com(g, topn=10, centrality=centr, grp_by="aff")
plot_important_actors(acts, centr)

# Overall important actors
top_acts = named_to_df(topn_by_centrality(g, centr, 10))
top_acts
dotline_plot(top_acts, "name", "val", xlab_ang=90) + ylab(centr)


# Media outlets
media_names = c("el_pais", "publico_es", "europapress", "voz_populi", 
                "SextaNocheTV", "eldiarioes", "abc_es", "elperiodico", 
                "elpais_espana", "ElHuffPost", "elmundoes",                  
                "eljueves", "la_informacion")

# Degree of media
media = sort(sapply(media_names, function(x) degree(g, x), USE.NAMES=F), decr=T)
dotline_plot(named_to_df(media), "name", "val") + ylab("degree")

# Affiliation of media
med_aff = sapply(names(media), function(x) vertex_attr(g, "aff", x))
med_aff = named_to_df(med_aff, alt_names=c("media", "affiliation"))
med_aff

# Break down neighbourhood
# Calculate count of members in each community and normalize 
# by maximum count and community size
rel_aff = function(g, node_name, grp_by="aff") {   
  node_aff = neighbour_affiliations(g, node_name, grp_by=grp_by, mode="in")
  gsizes = group_sizes(g, grp_by)
  gsizes_rel = gsizes[names(node_aff)] / max(gsizes[names(node_aff)])
  #aff_rel = node_aff / (max(node_aff) * gsizes_rel)
  aff_rel = node_aff / gsizes[names(node_aff)]
  sort(aff_rel, decr=T)
}
  
aff_vals = sapply(names(media), function(x) { rel_aff(g, x, grp_by="aff")})
dotline_arrange(aff_vals, ylab_str="aff", title="Affiliations for media outlets")

# Same data by party rather than media outlet
party_names = unique(names(party_affiliations))
byparty = t(vapply(aff_vals, FUN=function(X) unlist(X)[party_names], 
                   FUN.VALUE=numeric(length(party_names))))
byparty = byparty[, colSums(is.na(byparty)) < nrow(byparty)] # Remove parties with no data
byparty = data.frame(byparty)
byparty[is.na(byparty)] = 0
dotline_arrange_df(byparty)

# Gini with bootstrapped confidence intervals ("percentile bootstrap")
# http://stats.stackexchange.com/questions/21868/gini-coefficient-and-error-bounds
gini_errors = function(x, rep=1000) {
  g = gini(x)
  y = boot(x, gini, rep)
  q = quantile(y$t, probs=c(0.025, 0.975))
  data.frame(g=g, g_min=q[[1]], g_max=q[[2]])
}

gini_errors(byparty$podemos, rep=1000)
ginis = apply(byparty, 2, FUN=function(x) gini_errors(x))
ginis = do.call(rbind, ginis)
ginis$aff = rownames(ginis)
ginis$aff = factor(ginis$aff, levels=ginis[order(ginis$g), "aff"])  
ggplot(ginis, aes(x=aff, y=g)) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=g_min, ymax=g_max), width=0.2) +
  xllab("") + ylab("gini") + ggtitle("Inequality of media affiliation")
  theme_minimal()


# Above is not taking account of weights (i.e. number of retweets), 
# just count of retweeting accounts
# For tweet count: 1. get neighbourhood of node; 2. get edges from neighbours to node;
# 3. Add weights of edges

# Overlapping communities
start = Sys.time()
cliqs = clique_community_faster(g, 3)
elapsed = Sys.time() - start
elapsed