library(igraph)
source('graphutils.R')

root_dir = "~/Code/tweetonomy/"

tblnm = "parties"
data_dir = paste0(root_dir, "data/", tblnm, "/")
layers = list(R="retweet", M="mention", C="combined")
versions = list(C="catalunya", G="general", A="all")
periods = list(C=list(from="00000000", to="20151007"),
               G=list(from="20151001", to="99999999"),
               A=list(from="00000000", to="99999999"))


# Filename date format to date
# Filesnames have format YYYYmmdd
# --------------------------------------------------------------------
filename_to_date = function(filename) {
  strptime(filename, format="%Y%m%d")
}

# --------------------------------------------------------------------
cachename_for_graph = function(layer, version) {
  paste0(data_dir, "r-cache/", layers[layer], "-", versions[version], "-graph.Rdata")
}


# Return a variable stored in an external file
# --------------------------------------------------------------------
get_cached = function(varnm, fnm) {
  if (file.exists(fnm)) {
    local({
      load(fnm)
      if (exists(varnm)) get(varnm) else NULL
    })
  } else {
    print("File doesn't exists! Can't get cached variable.")
    return(NULL)
  }
}


# Fetches edge-list for given day
# --------------------------------------------------------------------
edge_list_day = function(day_str, layer) {   
  if (layer == "R") {
    fnm = paste0(data_dir, "retweet-edges/", day_str, ".txt")
  } else {
    fnm = paste0(data_dir, "mention-edges/", day_str, ".txt")      
  }
  colnames = c("from", "to", "weight")
  read.table(fnm, sep='\t', header=F, col.names=colnames, stringsAsFactors=F)
}


# --------------------------------------------------------------------
quick_edge_list = function(fnm) {   
  tryCatch(read.table(fnm, sep='\t', header=F, stringsAsFactors=F), error=function(e) {
    print(e)
    NULL
  })
}


# All day edge lists combined
# --------------------------------------------------------------------
combined_edge_list = function(layer, from="00000000", to="99999999") {
  fld = ifelse(layer=="R", "retweet-edges/", "mention-edges/")
  day_files = file_path_sans_ext(list.files(paste0(data_dir, fld)))
  day_files = day_files[!is.na(as.numeric(day_files))]
  day_files = day_files[(day_files >= from) & (day_files <= to)]
  day_files = paste0(data_dir, fld, day_files, ".txt")

  edge_list_df = do.call(rbind, lapply(day_files, function(day) { 
    print(sprintf("Loading day %s", day)); flush.console()
    quick_edge_list(day)
  }))
  names(edge_list_df) = c("from", "to", "weight")
  # Combine edges between identical pairs of accounts (from different days) by summing up their weights
  edge_list_df %>% 
    group_by(from, to) %>% 
    summarize(weight=sum(weight))
}


# Creates graph from edge-list stored in file
# --------------------------------------------------------------------
load_graph = function(fnm) {
  colnames = c("from", "to", "weight")
  edge_list = read.table(fnm, sep='\t', header=F, col.names=colnames, stringsAsFactors=F)
  graph.data.frame(edge_list)
}


# Get or create global graph
# --------------------------------------------------------------------
cache_global_graph = function(layer="R", version="C", com_algo, topn_coms=10, verbose=F) {
  from = periods[[version]]$from
  to = periods[[version]]$to
  print(sprintf("Generating global graph file for layer %s and version %s", layers[layer], versions[version]))
  if (layer %in% c("R", "M")) {
    edge_list = combined_edge_list(layer, from, to)
    G = graph.data.frame(edge_list) %>%
      simplify(edge.attr.comb=list(weight="sum"))
  } else {
    r_edges = combined_edge_list("R", from, to)
    #r_edges$type="R"
    m_edges = combined_edge_list("M", from, to)
    #m_edges$type="M"
    edge_list = rbind(r_edges, m_edges)
    G = graph.data.frame(edge_list) %>%
      simplify(edge.attr.comb=list(weight="sum")) # , type=function(x) paste(x, collapse="")
  }
  print("Identifying communities.")
  G$communities = find_communities(G, com_algo)
  V(G)$com = membership(G$communities)
  print("Determining affiliations.")
  V(G)$aff = affiliations_list(membership(G$communities), affmap=NULL, topn_grps=topn_coms)
  print("Done. Just saving graph now.")
  save(G, file=cachename_for_graph(layer, version))
  G
}


# --------------------------------------------------------------------
global_graph = function(layer, version, com_algo, topn_coms=10, recache=F, verbose=F) {
  if (recache) {
    return(cache_global_graph(layer, version, com_algo, topn_coms, verbose))
  } else {
    G = get_cached("G", cachename_for_graph(layer, version))
    if (is.null(G)) {
      G = cache_global_graph(layer, version, com_algo, topn_coms, verbose)
    }
    G
  }
}


# --------------------------------------------------------------------
cache_all_graphs = function(com_algo, topn_coms=10) {
  combs = apply(expand.grid(names(layers), names(versions)), 1, as.list)
  for (c in combs) {
    global_graph(c[[1]], c[[2]], com_algo, topn_coms)
  }
}


# Write for gephi
# --------------------------------------------------------------------
write_gephi = function(g, fnm) {
  write.graph(g, paste0(data_dir, "r-cache/", fnm), format="graphml")
}


# Read and preprocess per-user hashtags stored in file
# --------------------------------------------------------------------
hashtags = function(fnm) {
  tags = read.table(fnm, sep='\t', header=F, col.names=c("name", "hashtags"),
                    colClasses=c("character", "character"), encoding="UTF-8")
  
  tags$hashtags = tag_list(tags$hashtags, to_ascii=T)
  tags$num = sapply(tags$hashtags, length)
  return(tags)
}

# Read and return users for a certain day and their tweet count
# --------------------------------------------------------------------
day_users = function(day_str, layer) { 
  if (layer %in% c("R", "M")) {
    edges = edge_list_day(day_str, layer)
  } else {
    r_edges = edge_list_day(day_str, "R")
    m_edges = edge_list_day(day_str, "M")
    edges = rbind(r_edges, m_edges)
  }
  
  edges %>%
    group_by(from) %>%
    summarize(count=sum(weight)) %>%
    arrange(desc(count)) %>%
    rename(name=from)
}

