library(igraph)
library(tcltk)
library(rgl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

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

centrality_names = list("degree", "between", "eigen", "close", "auth", "hub", "pgrank")

centrality = function(graph, centrality='eigen', norm=T) {
  switch(centrality,
         degree = degree(graph, mode='all', normalized=norm),
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
  return(names(cent)[topi])
}


# Assumes graph has as a "com" vertex attribute (membership), and a
# communities object as graph attribute
# Return named list of actors per community, labelled by community index
# For flat list, unlist(result)
important_actors_by_com = function(graph, topn=2, centrality='res') {
  com_ids = unique(V(graph)$com)
  sel_coms = graph$communities[com_ids]
  group_acts = list()
  for (i in 1:length(sel_coms)) {
    com_name = names(sel_coms)[i]
    com_verts = V(h)[sel_coms[[com_name]]]  
    sub = induced_subgraph(graph, com_verts)  
    group_acts[[com_name]] = topn_by_centrality(sub, centrality, topn)
    remove(sub)
  }
  return(group_acts)
}

# --------------------------------------------------------------------------------
# Calculate extra features
# --------------------------------------------------------------------------------
g_plot = function(graph, layout=NULL, colorAttr=NULL, mode='r', emph_ids=NULL) {
  
  # Create layout
  if (is.null(layout)) {
    layout = with_fr(niter=1000)    
  }
  graph = add_layout_(graph, layout, component_wise(merge_method="dla"))
  
  # Add vertex colors
  if (is.null(colorAttr)) {
    if ("com" %in% list.vertex.attributes(graph)) {
      colorAttr = "com" } 
  }
  
  if (!is.null(colorAttr) && colorAttr %in% list.vertex.attributes(graph)) {
    graph = add_color_by_attribute(graph, colorAttr)
  } else {
    V(graph)$color = "black"
  }
  
  w_max = max(E(graph)$weight)
  ewidth = 5*E(graph)$weight/w_max
  vsize = 0.5+1.5*log10(degree(graph))
  vlfamily = "sans"
  vlcex = 0.75
  vldeg = 0
  vldist = 0.0
  
  # highlight vertices
  V(graph)$vertex.frame.color = NA
  if (is.vector(emph_ids)) {
    vsize[emph_ids] = vsize[emph_ids] * 2    
    #V(graph)$vertex.frame.color = NA
    V(graph)[emph_ids]$vertex.frame.color = "black"
  }
  
  if ("label" %in% list.vertex.attributes(graph)){
    labels = V(graph)$label
  } else {
    labels = NA
  }
  
  args = list(graph, 
              vertex.label=labels,
              vertex.size=vsize, vertex.label.color="black",
              vertex.frame.color=V(graph)$vertex.frame.color,
              vertex.label.family=vlfamily, vertex.label.cex=vlcex, 
              vertex.label.degree=vldeg, vertex.label.dist=vldist,
              edge.width=ewidth, edge.arrow.size=0, edge.arrow.width=0, 
              asp=0, margin=-0.1)
  
  if (mode == 'r') {
    do.call(plot, args)
  } else if (mode == 'tk') {
    tkid = do.call(tkplot, args)
    canvas = tk_canvas(tkid)
    tkconfigure(canvas, "bg"="white")
  } else if (mode == 'gl') {
    rgl.open()
    rgl.bg(color="white", alpha=c(.3), back="fill", sphere = FALSE, fogtype = "none", line_antialias = TRUE)
    rgl.viewpoint(0, 0, fov=100, zoom=.5)
    rgl.material(lit=FALSE, shininess=0)
    rgl.pop("lights")
    light3d(specular="black")
    do.call(rglplot, args)
  }
  
  return(graph)
}

# --------------------------------------------------------------------------------
# Calculate extra features
# --------------------------------------------------------------------------------
add_color_by_attribute = function(graph, attrib) {
  a = get.vertex.attribute(graph, attrib)
  com_fac = as.factor(a)
  num_facs = length(unique(levels(com_fac)))
  #palette = rainbow(num_facs)
  palette = brewer.pal(num_facs, "Set3")
  V(graph)$color = as.character(lapply(com_fac, function(x) { palette[x] }))
  edge.start = get.edges(graph, 1:ecount(graph))[,1]
  E(graph)$color = V(graph)$color[edge.start]
  return(graph)
}

add_importance = function(graph) {
  act = important_actors(h)
  V(graph)$importance = abs(act$res)
  return(graph)
}


# --------------------------------------------------------------------------------
# Modify weights to reflect community structure
# https://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph
# --------------------------------------------------------------------------------
edge_weight_community = function(row, membership, weigth.within, weight.between) {
  if(as.numeric(membership[which(names(membership)==row[1])])==as.numeric(membership[which(names(membership)==row[2])])){
    return(weigth.within)
  }else{
    return(weight.between)
  }
}

reweight_by_community = function(graph, within, between) {
  G = graph
  E(G)$weight = apply(get.edgelist(G), 1, edge_weight_community, membership(graph$communities), within, between)
  return(G)
}


# --------------------------------------------------------------------------------
# Wrapper for community detection algos
# http://bommaritollc.com/2012/06/summary-community-detection-algorithms-igraph-0-6/
# --------------------------------------------------------------------------------
find_communities = function(graph, algo, weights=NULL) {
  if (is.null(weights)) {
    weights = E(graph)$weight
  }
  
  if (algo == 'walktrap') {
    return (cluster_walktrap(graph, weights=weights))
  } else if (algo == 'edge_betweenness') {
    return (cluster_edge_betweenness(graph, weights=weights))
  } else if (algo == 'fast_greedy') {
    gu = as.undirected(graph, mode="collapse", edge.attr.comb=list(weight="sum"))
    return (cluster_fast_greedy(gu, weights=E(gu)$weights))
  } else if (algo == 'spinglass') {
    return (cluster_spinglass(graph, weights=weights))
  } else if (algo == 'leading_eigen') {
    return (cluster_leading_eigen(graph, weights=weights))
  } else if (algo == 'optimal') {
    return (cluster_optimal(graph, weights=weights))
  } else if (algo == 'louvain') {
    return (cluster_louvain(graph, weights=weights))
  } else if (algo == 'label_prop') {
    return (cluster_label_prop(graph, weights=weights))
  } else if (algo == 'infomap') {
    return (cluster_infomap(graph, e.weights=weights))
  }
}

community_containing = function(graph, nodename) {
  com_ids = unique(V(graph)$com)
  for (i in 1:length(com_ids)) {
    com_id = com_ids[i]
    if (nodename %in% graph$communities[[com_id]]) {
      return(com_id)
    }
  }
  return(-1)
}

# --------------------------------------------------------------------------------
# Graph filters
# --------------------------------------------------------------------------------

# Only large communities
# --------------------------------------------------------------------------------
filter_topn_communities = function(graph, n, algo="fast_greedy", coms=NULL) {
  if(is.null(coms)){
    graph$communities = find_communities(graph, algo)
    V(graph)$com = membership(graph$communities)
  }
  top_coms = names(sort(sizes(graph$communities), decreasing=T)[1:n])
  excl_verts = !(membership(graph$communities) %in% top_coms)
  return(delete.vertices(graph, excl_verts))
}

filter_min_community_size = function(graph, min_size, algo="fast_greedy", coms=NULL) {
  if(is.null(coms)){
    graph$communities = find_communities(graph, algo)
    V(graph)$com = membership(graph$communities)
  }
  com_sizes = sizes(graph$communities)
  big_coms = names(com_sizes[com_sizes >= min_size])
  excl_verts = !(membership(graph$communities) %in% big_coms)
  return(delete.vertices(graph, excl_verts))
}

# Maximal subgraph in which each vertex has at least degree k
filter_min_coreness = function(graph, min_coreness=3) {
  vcoreness = coreness(graph, mode="all")
  kcore = induced_subgraph(graph, as.vector(which(vcoreness >= min_coreness)))
  return(kcore)
}

# Connected components
# https://lists.nongnu.org/archive/html/igraph-help/2011-08/msg00000.html
filter_small_components = function(graph, min_size=3, mode="weak") {
  comps = components(graph, mode)
  small_components = which(comps$csize < min_size)
  excl_verts = which(comps$membership %in% small_components)
  return(delete.vertices(graph, excl_verts))
}

filter_topn_components = function(graph, topn=1, mode="weak") {
  comps = components(graph, mode)
  topi = order(comps$csize, decreasing=T)[1:topn]
  incl_verts = which(comps$membership %in% topi)
  return(induced_subgraph(graph, incl_verts))
}

filter_isolates = function(graph) {
  return(delete.vertices(graph, which(degree(graph) == 0))) 
}

# --------------------------------------------------------------------------------
# Layouts that reflect community structure
# --------------------------------------------------------------------------------

# https://stackoverflow.com/questions/28715736/how-to-spread-out-community-graph-made-by-using-igraph-package-in-r
# --------------------------------------------------------------------------------
layout.by.attr = function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g = graph.edgelist(get.edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight = 1  
  attr = cbind(id=1:vcount(g), val=wc)
  g = g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)  
  l = layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

# https://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph
# --------------------------------------------------------------------------------
layout.modular = function(G, c) {
  G$layout = layout.fruchterman.reingold(G) # Fill with something so not NULL
  nm = length(levels(as.factor(c$membership)))
  gr = 2
  while(gr^2 < nm) {
    gr = gr + 1
  }
  
  i = j = 0
  for(cc in levels(as.factor(c$membership))) {
    F = delete.vertices(G, c$membership != cc)
    F$layout = layout.kamada.kawai(F)
    F$layout = layout.norm(F$layout, i, i+0.5, j, j+0.5)
    G$layout[c$membership==cc,] = F$layout
    if(i==gr){
      i = 0
      if(j==gr){
        j = 0
      }else{
        j = j+1
      }
    }else{
      i = i+1
    }
  }
  return(G$layout)
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

# communities
coms = find_communities(g, 'fast_greedy')
head(sort(sizes(coms), decreasing=T), 25)
