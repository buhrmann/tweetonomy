library(network)
library(igraph)
library(networkD3)
library(tcltk)
library(rgl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringi)

vertex_attribs = function(graph, ids, sort_by=NULL){
  df = data.frame(sapply(vertex_attr_names(graph),function(x) vertex_attr(graph, x, index=ids)))
  if (!(is.null(sort_by))) {
    df = df[order(-as.numeric(df[[sort_by]])),]
  }
  return(df)
}

# --------------------------------------------------------------------------------
# Basic stats
# --------------------------------------------------------------------------------
component_sizes = function(graph, mode="weak") {
  comps = components(graph, mode)
  table(comps$csize)
}

kcore_dist = function(graph) {
  vcoreness = coreness(graph)
  table(vcoreness)
}

weight_dist = function(graph) {
  table(E(graph)$weight)
}

centrality_names = list("degree", "indeg", "outdeg", "between", "eigen", "close", "auth", "hub", "pgrank")

centrality = function(graph, centrality='eigen', norm=T) {
  switch(centrality,
         degree = degree(graph, mode='all', normalized=norm),
         indeg = degree(graph, mode='in', normalized=norm),
         outdeg = degree(graph, mode='out', normalized=norm),
         between = betweenness(graph, normalized=norm), 
         eigen = eigen_centrality(graph, directed=T, scale=norm)$vector,
         close = closeness(graph, mode='all'),
         auth = authority_score(graph, scale=norm)$vector,
         hub = hub_score(graph)$vector,
         pgrank = page_rank(graph)$vector 
         )
}

centralities = function(graph, norm=TRUE, plot=FALSE) {
  cents = lapply(centrality_names, FUN=centrality, graph=graph, norm=norm)
  cents = as.data.frame(do.call(cbind, cents))
  colnames(cents) = centrality_names
  
  # Drew Conway: high bet and low eig vs. low bet and high eig...
  cents$res = abs(lm(eigen~between, data=cents)$residuals)
  if (plot) {
    cents$names = rownames(cents)
    ggplot(cents, aes(x=bet, y=eig, color=res, label=names, size=abs(res))) + 
      geom_text() +
      xlab("Betweenness") + ylab("EV")
  }
  return(cents)
}


topn_by_centrality = function(graph, centrality="eigen", topn=5) {
  cent = centrality(graph, centrality, norm=F)
  topi = order(cent, decreasing=T)[1:topn]
  cent[topi]
}


# Assumes graph has as a "com" vertex attribute (membership), and a
# communities object as graph attribute
# Return named list of actors per community, labelled by community index
# For flat list, unlist(result)
important_actors_by_com = function(graph, topn=2, centrality='res', grp_by="com") {
  grp_names = names(sort(group_sizes(graph, grp_by), decr=T)) # nice ordering by size
  group_acts = list()
  for (name in grp_names) {
    com_verts = which(vertex_attr(graph, grp_by) == name)
    sub = induced_subgraph(graph, com_verts)  
    group_acts[[name]] = topn_by_centrality(sub, centrality, topn)
    remove(sub)
  }
  return(group_acts)
}

important_actors_to_df = function(ia) {
  return(cbind.data.frame(lapply(ia, names)))
}

# Calculates the topn_per actors for each community and each centrality measure
# Then returns the topn based on the rank of each actor, i.e. after calculating
# how many times an actors appeared in the topn_per centralities
important_actors_overall = function(graph, topn_per=10, topn=3) {
  combined = do.call(rbind, lapply(centrality_names, FUN=function(x) {
    important_actors_to_df(important_actors_by_com(graph, topn_per, x));
  }))
  topn_by_com = sapply(combined, FUN=function(x){names(sort(table(x), decr=T))[1:topn]})
  return(as.data.frame(topn_by_com))
}


# --------------------------------------------------------------------------------
# Wrapper for community detection algos
# http://bommaritollc.com/2012/06/summary-community-detection-algorithms-igraph-0-6/
# --------------------------------------------------------------------------------
community_algo_names = c('walktrap', 'edge_betweenness', 'fast_greedy','spinglass',
                        'leading_eigen', 'optimal', 'louvain', 'label_prop', 'infomap', 'multilevel')

undirected = function(graph) {
  as.undirected(graph, mode="collapse", edge.attr.comb=list(weight="sum"))
}

find_communities = function(graph, algo) {
  weights = E(graph)$weight
  switch(algo,
         walktrap = cluster_walktrap(graph, weights=weights),
         edge_betweenness = cluster_edge_betweenness(graph, weights=weights),
         fast_greedy = cluster_fast_greedy(undirected(graph)),
         spinglass = cluster_spinglass(graph, weights=weights),
         leading_eigen = cluster_leading_eigen(graph, weights=weights),
         optimal = cluster_optimal(graph, weights=weights),
         louvain = cluster_louvain(undirected(graph)),
         label_prop = cluster_label_prop(graph, weights=weights),
         infomap = cluster_infomap(graph, e.weights=weights),
         # Next same as louvain ???
         multilevel = multilevel.community(undirected(graph)),
         clique = clique_community_faster(graph, 3)
  )
}


# https://lists.nongnu.org/archive/html/igraph-help/2011-04/msg00041.html
clique_community_faster = function(graph, k) {
  clq = maximal.cliques(graph)
  clq = clq[lapply(clq, length) >= k]
  edges = c()
  for (i in seq_along(clq)) {
    
    for (j in seq_along(clq)) {
      if (length(intersect(clq[[i]], clq[[j]])) >= k-1) {
        edges = c(edges, c(i,j) - 1)
      }
    }
  }
  clq.graph = simplify(graph(edges))
  V(clq.graph)$name = seq_len(vcount(clq.graph))
  comps = decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}


# http://igraph.wikidot.com/community-detection-in-r
clique_community = function(graph, k) {
  clq = cliques(graph, min=k, max=k)
  edges = c()
  for (i in seq_along(clq)) {
    for (j in seq_along(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        edges = c(edges, c(i,j)-1)
      }
    }
  }
  clq.graph = simplify(graph(edges))
  V(clq.graph)$name = seq_len(vcount(clq.graph))
  comps = decompose.graph(clq.graph)
  
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}
         

attr_of = function(graph, nodename, attr="com") {
  vertex_attr(graph, attr, nodename)
}


# Calculates sized of actually remaining communities in graph (e.g. after filtering)
group_sizes = function(graph, grp_by="com", index=V(graph)) {
  sort(table(vertex_attr(graph, grp_by, index)), decreasing=T)
}

# All vertices with the same attribute value
get_group = function(graph, val, grp_by="com") {
  V(graph)[which(vertex_attr(graph, grp_by) == val)]
}

# Same as above but operating on named lists (e.g. community membership)
group_sizes_list = function(named_list) {
  sort(table(named_list), decreasing=T)
}

get_group_list = function(named_list, val) {
  named_list[which(named_list == val)]
}

# --------------------------------------------------------------------------------
# Graph filters
# --------------------------------------------------------------------------------

# Only large communities
# --------------------------------------------------------------------------------
filter_topn_communities = function(graph, n, algo="fast_greedy", comname="com") {
  if(is.null(graph$communities)){
    print("Calculating communities...")
    graph$communities = find_communities(graph, algo)
    V(graph)$com = membership(graph$communities)
  }
  com_sizes = group_sizes(graph, comname)
  top_coms = names(sort(com_sizes, decreasing=T)[1:n])
  incl_verts = which(vertex_attr(graph, comname) %in% top_coms)
  induced_subgraph(graph, incl_verts)
}

filter_min_community_size = function(graph, min_size, algo="fast_greedy", comname="com") {
  if(is.null(graph$communities)){
    print("Calculating communities...")
    graph$communities = find_communities(graph, algo)
    V(graph)$com = membership(graph$communities)
  }
  com_sizes = group_sizes(graph, comname)
  big_coms = names(com_sizes[com_sizes >= min_size])
  incl_verts = which(vertex_attr(graph, comname) %in% big_coms)
  induced_subgraph(graph, incl_verts)
}

# Maximal subgraph in which each vertex has at least degree k
filter_min_coreness = function(graph, min_coreness=3) {
  vcoreness = coreness(graph, mode="all")
  induced_subgraph(graph, as.vector(which(vcoreness >= min_coreness)))
}

# Connected components
# https://lists.nongnu.org/archive/html/igraph-help/2011-08/msg00000.html
filter_small_components = function(graph, min_size=3, mode="weak") {
  comps = components(graph, mode)
  small_components = which(comps$csize < min_size)
  excl_verts = which(comps$membership %in% small_components)
  delete.vertices(graph, excl_verts)
}

filter_topn_components = function(graph, topn=1, mode="weak") {
  comps = components(graph, mode)
  topi = order(comps$csize, decreasing=T)[1:topn]
  incl_verts = which(comps$membership %in% topi)
  induced_subgraph(graph, incl_verts)
}

filter_attr_in = function(graph, attr, attr_vals) {
  incl_verts = which(vertex_attr(graph, attr) %in% attr_vals)
  induced_subgraph(graph, incl_verts)
}

filter_min_degree = function(graph, min_deg=2) {
  delete_vertices(graph, which(degree(graph) < min_deg))
}

filter_isolates = function(graph) {
  filter_min_degree(graph, min_deg=1)
}

filter_min_weight = function(graph, min_w=2) {
  delete_edges(graph, which(edge_attr(graph, "weight") < min_w))  
}


# --------------------------------------------------------------------------------
# Affiliation: Identify groups of nodes
# --------------------------------------------------------------------------------
party_affiliations = list("podemos"=c("ahorapodemos", "Pablo_Iglesias_", "ierrejon"), 
                    "catalunya"=c("CatalunyaNoPots", "HiginiaRoig"), 
                    "ciudadanos"=c("Albert_Rivera", "CiudadanosCs"), 
                    "pp"=c("marianorajoy", "PPopular"), 
                    "psoe"=c("PSOE", "gpscongreso"), 
                    "iu"=c("cayo_lara", "iunida"), 
                    "upyd"=c("UPyD", "Csilva2Carlos", "cmgorriaran"),
                    "vox"=c("vox_baracaldo", "vox_guipuzcoa"),
                    "prensa"=c("el_pais", "europapress", "20m"),
                    "prensa"=c("el_pais", "ElHuffPost", "eljueves"),
                    "izquierda"=c("BeatrizTalegon") )


# Checks if members of affiliations list are all inside grp
group_affiliation = function(grp, affiliations=party_affiliations) {
  for (aff in names(affiliations)) {
    if (all(affiliations[[aff]] %in% names(grp))) {
      return(aff)
    }
  }
  return("unknown")
}


# Map of grpname to affiliation name
affiliation_map = function(graph, grp_by="com", affiliations=party_affiliations) {
  grpnames = names(group_sizes(graph, grp_by))
  sapply(grpnames, function(x) group_affiliation(get_group(graph, x, grp_by), affiliations))
}

# Get affiliations for each node
affiliations = function(graph, grp_by="com", affiliations=party_affiliations) {
  affmap = affiliation_map(graph, grp_by, affiliations)
  sapply(as.character(vertex_attr(graph, grp_by)), 
         function(grpval) affmap[[grpval]], USE.NAMES=F)
}

# Assigns new columns with affiliation names
set_affiliations = function(graph, grp_by="com", aff_attr="aff", affiliations=party_affiliations) {
  affs = affiliations(graph, grp_by, affiliations)
  set_vertex_attr(graph, aff_attr, value=affs)
}

# Breakdown of a node's neighbour's affiliation
neighbour_affiliations = function(graph, node, grp_by="aff", mode="all") {
  n = neighbors(graph, node, mode=mode)
  group_sizes(graph, grp_by, n)
}

# As above for named membership list, rather than graph
affiliation_map_list = function(named_list, affiliations=party_affiliations) {
  grpnames = names(group_sizes_list(named_list))
  sapply(grpnames, function(x) group_affiliation(get_group_list(named_list, x), affiliations))
}

affiliations_list = function(named_list, affmap=NULL) {
  if (is.null(affmap))
    affmap = affiliation_map_list(named_list)
  sapply(as.character(named_list), function(grpval) affmap[[grpval]], USE.NAMES=F)
}


# --------------------------------------------------------------------------------
# Print basic stats
# --------------------------------------------------------------------------------
print_stats = function(g) {
  component_sizes(g, "weak")
  kcore_dist(g)
  vcount(g)
  ecount(g)
}

# --------------------------------------------------------------------------------
# Content analysis helpers
# --------------------------------------------------------------------------------

# comma-separated array of hashtags to list
tag_list = function(str, to_ascii=TRUE) {
  l = strsplit(gsub("[\\[\\]]", "", str, perl=T), ',')
  if (to_ascii) {
    l = sapply(l, function(x) stri_trans_general(x, "Latin-ASCII"))
  }
  l
}

# Remove most frequent items from list of lists (lol)
rm_topn = function(lol, topn) {
  item_freq = sort(table(unlist(lol)), decr=T)
  common_items = names(item_freq[1:topn])
  sapply(lol, FUN=function(lst) { 
    items = unlist(lst);
    items[!(items %in% common_items)] 
  })
}


