library(igraph)
source('utils.R')

# Return a variable stored in an external file
get_cached = function(varnm, fnm) {
  if (file.exists(fnm)) {
    local({
      load(fnm)
      if (exists(varnm)) get(varnm) else NULL
    })
  } else {
    print("File doesn't exists! Returning NULL.")
    return(NULL)
  }
}


# Creates graph from edge-list stored in file
load_graph = function(fnm) {
  colnames = c("from", "to", "weight")
  edge_list = read.table(fnm, sep='\t', header=F, col.names=colnames)
  graph.data.frame(edge_list)
}


# Return cached communities or create from all tweets
global_communities = function(cache_fnm, edgelist_fnm, algo) {
  global_coms = get_cached("global_coms", cache_fnm)
  if (is.null(global_coms)) {
    print("Generating global coms file.")
    gg = load_graph(edgelist_fnm)
    global_coms = find_communities(gg, algo)
    save(global_coms, file=cache_fnm)
    rm(gg)
  }
  return(global_coms)
}

global_graph = function(cache_fnm, edgelist_fnm, com_algo) {
  G = get_cached("G", cache_fnm)
  if (is.null(G)) {
    print("Generating global graph file.")
    G = load_graph(edgelist_fnm)
    G$communities = find_communities(G, com_algo)
    V(G)$com = membership(G$communities)
    V(G)$aff = affiliations_list(membership(G$communities))
    save(G, file=cache_fnm)
  }
  return(G)
}


# Read and preprocess per-user hashtags stored in file
hashtags = function(fnm) {
  tags = read.table(fnm, sep='\t', header=F, col.names=c("name", "hashtags"),
                    colClasses=c("character", "character"), encoding="UTF-8")
  
  tags$hashtags = tag_list(tags$hashtags, to_ascii=T)
  tags$num = sapply(tags$hashtags, length)
  return(tags)
}