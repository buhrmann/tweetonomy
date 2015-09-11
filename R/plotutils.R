library(network)
library(igraph)
library(networkD3)
library(tcltk)
library(rgl)
library(ggplot2)
library(RColorBrewer)

# --------------------------------------------------------------------------------
# Plotting helpers
# --------------------------------------------------------------------------------

# IGraph plots
# --------------------------------------------------------------------------------
g_plot = function(graph, layout=NULL, colorAttr=NULL, mode='r', emph_ids=NULL) {
  
  # Create layout
  if (is.null(layout)) {
    layout = with_fr(niter=1000)    
  }
  graph = add_layout_(graph, layout, component_wise(merge_method="dla"))
  
  # Add vertex colors
  if (is.null(colorAttr)) {
    if ("com" %in% vertex_attr_names(graph)) {
      colorAttr = "com" 
    } else if ("global_com" %in% vertex_attr_names(graph)) {
      colorAttr = "global_com"
    }
  }
  
  if (!is.null(colorAttr) && colorAttr %in% vertex_attr_names(graph)) {
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
    #vsize[emph_ids] = vsize[emph_ids] * 2    
    #V(graph)$vertex.frame.color = NA
    V(graph)[emph_ids]$vertex.frame.color = "black"
  }
  
  if ("label" %in% vertex_attr_names(graph)){
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


# Network package plots
# --------------------------------------------------------------------------------
n_plot = function(graph) {
  edges = get.edgelist(graph)
  #nodes = cbind(idn=factor())
  net = network(edges, matrix.type="edgelist")
  net %v% "com" = V(graph)$com
  #layout = network.layout.fruchtermanreingold(net, layout.par=)
  plot(net, usearrows=F, displaylabels=F, bg="#000000",
       vertex.col=net %v% "com", vertex.cex=0.5, edge.col='#555555',       
       mode="fruchtermanreingold")
}


# D3 plots
# --------------------------------------------------------------------------------
d3_nodes_edges = function(graph) {
  dfs = get.data.frame(graph, what="both")
  links = data.frame(
    "source" = match(dfs$edges$from, dfs$vertices$name) - 1,
    "target" = match(dfs$edges$to, dfs$vertices$name) - 1,
    "weight" = dfs$edges$weight
  )
  nodes = data.frame(
    id = 0:(nrow(dfs$vertices)-1),
    name = dfs$vertices$name,
    group = dfs$vertices$com,
    degree = degree(graph)
  )
  return(list("links"=links, "nodes"=nodes))
}

# --------------------------------------------------------------------------------
d3_plot = function(graph) {
  
  dfs = d3_nodes_edges(graph)
  forceNetwork(Links=dfs$links, Nodes=dfs$nodes, 
               Source="source", Target="target", Value="weight", 
               NodeID="name", Group="group", Nodesize="degree",
               opacity=1.0, zoom=T, fontSize=14)
}


# --------------------------------------------------------------------------------
# Calculate extra features
# --------------------------------------------------------------------------------
add_color_by_attribute = function(graph, attrib) {
  a = vertex_attr(graph, attrib)
  com_fac = as.factor(a)
  num_facs = length(unique(levels(com_fac)))
  #palette = rainbow(num_facs)
  palette = brewer.pal(num_facs, "Set3")
  V(graph)$color = as.character(lapply(com_fac, function(x) { palette[x] }))
  edge.start = ends(graph, 1:ecount(graph))[,1]
  E(graph)$color = V(graph)[edge.start]$color
  return(graph)
}


# --------------------------------------------------------------------------------
# Stats, graphs etc.
# --------------------------------------------------------------------------------

named_to_df = function(named_list, decr=T, alt_names=c("name", "val")) {
  df = data.frame(name=names(named_list), val=named_list, row.names=NULL)  
  df$name = factor(df$name, levels=df[order(df$val, decreasing=decr), "name"])  
  names(df) = alt_names
  df
}

# dotplot (barplot alternative)
dotline_plot = function(df, xstr, ystr, log_scale=NULL, xlab_ang=45, xlab_s=1.0) {
  if (xlab_ang == 45) {
    hj = 1; vj=1;
  } else if (xlab_ang == 90) {
    hj = 1; vj = 0.5;
  }
  else {
    hj = 0.5; vj = 0.5;
  } 
    
  p = ggplot(df, aes_string(x=xstr, y=ystr)) + 
    geom_point(stat="identity", size=3) + 
    geom_linerange(aes_string(x=xstr, ymin=0, ymax=ystr), width=0.1) +
    theme_minimal() +
    theme(legend.position="none", 
          axis.text.x=element_text(size=rel(xlab_s), angle=xlab_ang, hjust=hj, vjust=vj), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    xlab("")
  
  if (!is.null(log_scale)) {
    if (grepl("x", log_scale))
      p = p + scale_x_log10()
    if (grepl("y", log_scale))
      p = p + scale_y_log10()
  }
  p
}


# Group sizes
plot_group_sizes = function(g, grp_by="aff", log_scale=NULL, topn=10) {
  gsizes = head(group_sizes(g, grp_by=grp_by), topn)
  df = named_to_df(gsizes)
  dotline_plot(df, "name", "val", log_scale) 
}


# Arrange a number of dotline plots, one for each element in named list
dotline_arrange_nl = function(named_list, ylab_str="", title="") {
  plts = lapply(seq_along(named_list), function(i) { 
    dotline_plot(named_to_df(named_list[[i]]), "name", "val", xlab_ang=45) + 
      ggtitle(names(named_list)[i]) +
      ylab(ylab_str)
  })
  do.call(grid.arrange, c(plts, main=title))
}


# Arrange a number of dotline plots, one for each col in df
dotline_arrange_df = function(df, ylab_str="", title="", ylim=c(0, NA)) {
  plts = lapply(1:ncol(df), function(i) { 
    coldf = data.frame(name=rownames(df), val=df[,i])
    #coldf$name = reorder(coldf$name, df$val, FUN=function(x) -1*)
    coldf$name = factor(coldf$name, levels=coldf[order(coldf$val, decreasing=TRUE), "name"])  
    dotline_plot(coldf, "name", "val", xlab_ang=45) + 
      ggtitle(colnames(df)[i]) +
      ylab(ylab_str) + scale_y_continuous(limits=ylim)
  })
  do.call(grid.arrange, c(plts, main=title))
}


plot_important_actors = function(coms_list, centr_str="page-rank") {
  plts = lapply(seq_along(coms_list), function(i) { 
    dotline_plot(named_to_df(coms_list[[i]]), "name", "val", xlab_ang=45) + 
      ggtitle(names(coms_list)[i]) +
      ylab(centr_str)
    })
  do.call(grid.arrange, c(plts, main="Top n centralities per community"))
}
