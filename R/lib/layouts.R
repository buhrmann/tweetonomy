library(igraph)
source("graphutils.R")

# --------------------------------------------------------------------------------
# Layouts that reflect community structure
# --------------------------------------------------------------------------------

# https://stackoverflow.com/questions/28715736/how-to-spread-out-community-graph-made-by-using-igraph-package-in-r
# Assumes graph vertices have name attribute !
# --------------------------------------------------------------------------------
layout_by_attr = function(graph, attr="aff", strength=1, layout=layout_with_drl, ...) {  
  g = graph_from_edgelist(as_edgelist(graph)) # create a lightweight copy of graph w/o the attributes.
  E(g)$weight = 1  
  #attr = cbind(name=1:vcount(g), val=wc)
  grp_attr = cbind(name=V(graph)$name, vertex_attr(graph, attr))
  g = g + vertices(unique(grp_attr[,2])) 
  g = g + edges(unlist(t(grp_attr)), weight=strength)  
  layout(g, weights=V(g)$weight, ...)[1:vcount(graph),]
}

# https://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph
# --------------------------------------------------------------------------------
layout_modular_grid = function(g, grp_by, width=0.5, weight_fun=NULL, layout, ...) {
  #g$layout = layout.fruchterman.reingold(g) # Fill with something so not NULL
  g$layout = matrix(0, nrow=vcount(g), ncol=2)
  nm = length(levels(as.factor(vertex_attr(g, grp_by))))
  gr = 2
  while(gr^2 < nm) {
    gr = gr + 1
  }
  
  i = j = 0
  for(cc in levels(as.factor(vertex_attr(g, grp_by)))) {
    H = filter_attr_in(g, grp_by, cc)
    if (!is.null(weight_fun)) {
      H$layout = layout(H, weights=weight_fun(E(H)$weight), ...)
    } else {
      H$layout = layout(H, ...)
    }
    H$layout = layout.norm(H$layout, i, i+width, j, j+width)
    g$layout[vertex_attr(g, grp_by)==cc,] = H$layout
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
  return(g$layout)
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
