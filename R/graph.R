library(igraph)
library(networkD3)
library(tcltk)
library(rgl)
library(utils)

# Graph from edge lists
colnames = c("from", "to", "weight")
edge_list = read.table("~/Code/tweetonomy/data/retweets.txt", sep='\t', header=T, col.names=colnames)
edge_list_fil = edge_list[edge_list$weight > 1,]

lout1 = layout.fruchterman.reingold
lout2 = layout.kamada.kawai

# igraph
# --------------------------------------------------------------------
g = graph.data.frame(edge_list)
hist(degree(g))

# Identify communities
gu = as.undirected(g, mode="collapse", edge.attr.comb=list(weight="sum"))
coms = cluster_fast_greedy(gu)
com_sizes = sizes(coms)
memship = membership(coms) #coms$membership     # community id for each vertex
V(g)$com = memship
coms_list = communities(coms) # list of communities by id and array for member vertices
cross = crossing(coms, gu)
print(modularity(gu, memship))

# Too big usually
# plot(coms, gu, col=memship, mark.groups=coms_list, 
#      edge.color=c("black", "red")[cross + 1], 
#      vertex.size=3, vertex.label=NA, edge.arrow.size=0)


# Simplify
gs = delete.vertices(g, which(degree(g) < 2)) 
gs = delete.vertices(gs, which(degree(gs) == 0)) 
V(gs)$color = as.character(lapply(as.factor(V(gs)$com), function(x) {col_pal[x]}))
plot(gs, vertex.size=3, vertex.label=NA,
     #edge.width=E(gs)$weight, 
     edge.arrow.size=0, edge.arrow.width=0, 
     #layout=lout1(gs, weights=E(gs)$weight))
     layout=layout.lgl)


# Remove vertices belonging to small communities
head(sort(com_sizes, decreasing=T), 20)
min_com_size = 200
big_coms = names(com_sizes[com_sizes > min_com_size])
vert_excl = !(memship %in% big_coms)
sum(vert_excl)
g.lcoms = delete.vertices(g, vert_excl)
vcount(g.lcoms)
col_pal = colorRampPalette(c("red","green"))(length(big_coms))
V(g.lcoms)$color = as.character(lapply(as.factor(V(g.lcoms)$com), function(x) {col_pal[x]}))
plot(g.lcoms, vertex.size=3, vertex.label=NA, vertex.color=V(g.lcoms)$color,
     edge.width=E(g.lc)$weight, edge.arrow.size=0, edge.arrow.width=0, 
     #layout=lout1(g.lcoms, weights=E(g.lcoms)$weight))
     layout=layout.drl)


# Modify weights to better reflect community structure in layout
# Not working!
g.rw = reweight_by_community(g.lcoms, memship, 10000, 1)
g.rw$layout = layout.fruchterman.reingold(g.rw, weights=E(g.rw)$weight)
plot(g.rw, vertex.size=3, vertex.label=NA, vertex.color=V(g.rw)$color,
     edge.width=1, edge.arrow.size=0, edge.arrow.width=0)


# Draw opengl
rgl.open()
rgl.bg(color="grey", alpha=c(.3), back="fill", sphere = FALSE, fogtype = "none", line_antialias = TRUE)
rgl.viewpoint(0, 0, fov=100, zoom=.5)
rglplot(g.lcoms, vertex.size=3, vertex.label=NA, #layout=layout.kamada.kawai, 
        edge.arrow.size=0, edge.arrow.width=0, edge.width=E(g.lcoms)$weight/2, edge.color="white")

# Identify cliques
lc = largest.cliques(g)
g.lc = induced_subgraph(g, lc[[3]], impl="auto")
plot(g.lc, vertex.size=degree(g.lc)/2, vertex.label=NA, edge.width=E(g.lc)$weight,
     edge.arrow.size=0, edge.arrow.width=0, layout=lout)

# Remove nodes belonging to smallest communities
k = 3
vcoreness = coreness(g, mode="all")
#smallfrys = which(vcoreness < k)
#kcore = delete.vertices(g, smallfrys)
kcore = induced_subgraph(g, as.vector(which(vcoreness >= k)))
col_pal = colorRampPalette(c("red","green"))(length(big_coms))
V(kcore)$color = as.character(lapply(as.factor(V(kcore)$com), function(x) {col_pal[x]}))
plot(kcore, vertex.size=degree(kcore)/2, vertex.label=NA, edge.width=E(kcore)$weight,
     edge.arrow.size=0, edge.arrow.width=0, layout=lout2(kcore, niter=1000))

# Simplified community graph
# Identify communities and then collapse
c = contract.vertices(g, memship)
c = simplify(c, edge.attr.comb=list(weight="sum"))
c = delete.vertices(c, which(degree(c) < 10)) 
hist(degree(c))
plot(c, vertex.size=2, vertex.label=NA, edge.width=E(c)$weight)
tkid = tkplot(c, vertex.size=2, vertex.label=NA, layout=layout.kamada.kawai)
canvas = tk_canvas(tkid)
tkconfigure(canvas, "bg"="white")

#opengl
rgl.open()
rgl.bg(color="grey", alpha=c(.3), back="fill", sphere = FALSE, fogtype = "none", line_antialias = TRUE)
rgl.viewpoint(0, 0, fov=100, zoom=.5)
rglplot(c, vertex.size=degree(c)+2, vertex.label=NA, #layout=layout.kamada.kawai, 
        edge.arrow.size=0, edge.arrow.width=0, edge.width=E(c)$weight/2, edge.color="white")

rglplot(g, vertex.size=3, vertex.label=NA, #layout=layout.kamada.kawai, 
        edge.arrow.size=0, edge.arrow.width=0, edge.width=E(g)$weight/2, edge.color="white")


gs = graph.data.frame(edge_list_fil)
E(gs)$weight = edge_list_fil$weight
E(gs)$color = "grey"
E(gs)[weight>5]$color = "red"
plot(gs, vertex.size=2, vertex.label=NA, layout=layout.fruchterman.reingold)
tkplot(gs, vertex.size=2, vertex.label=NA, layout=layout.fruchterman.reingold)


# networkD3
# --------------------------------------------------------------------
# Conversion
edge_list_to_dfs = function(edge_list) {
  unique_nids = unique(c(as.character(edge_list$from), as.character(edge_list$to)))
  src = as.numeric(factor(edge_list$from, levels=unique_nids))
  tgt = as.numeric(factor(edge_list$to, levels=unique_nids))
  link_df = data.frame(source=src, target=tgt, weight=edge_list$weight)
  
  node_ids = as.numeric(factor(unique_nids, levels=unique_nids))
  node_df = data.frame(id=node_ids)
  node_df$group = 1
  return(list("links"=link_df, "nodes"=node_df))
}


dfs = edge_list_to_dfs(edge_list_fil)
ig = graph.data.frame(dfs$links)
plot(ig, vertex.size=2, vertex.label=NA)
simpleNetwork(edge_list_fil)
forceNetwork(Links=dfs$links, Nodes=dfs$nodes, Source="source", Target="target", Value="weight",
             NodeID="id", Group="group", opacity=0.8)

