library(igraph)

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
