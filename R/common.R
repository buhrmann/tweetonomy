library(igraph)
library(tools)
library(utils)

# Working directory
source('utils.R')
source('fileutils.R')
source('plotutils.R')
source('mediautils.R')

# Define paths


# Pretty print an integer with thousands separator
# ------------------------------------------------------------------------------------------
pp_int = function(i) {
  formatC(i, big.mark=".", format="f", drop0trailing=T)
}


# Get a "clean" global graph
# ------------------------------------------------------------------------------------------
preprocess_graph = function(g, min_w=1) {
  g %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_min_weight(min_w=min_w) %>%
    filter_topn_components(topn=1, mode="weak") %>%    
    filter_isolates()  
}
  
process_ggraph = function(mode="R") {
  G = global_graph(cachename_for_graph(mode), community_algo) 
  preprocess_graph(G)
}


# Get day graph with affiliations from global graph
# --------------------------------------------------------------------
day_graph = function(day_str, mode, com_algo, G=NULL) {
  if (is.null(G)) {
    G = process_ggraph(mode=mode)
  }
  
  if (mode == "R") {
    g = load_graph(paste0(data_dir, "retweet-edges/", day_str, ".txt"))
  } else {
    g = load_graph(paste0(data_dir, "mention-edges/", day_str, ".txt"))
  }
  
  g = filter_in_other(g, G)
  
  V(g)$degree = degree(g)
  g$communities = find_communities(g, com_algo)
  V(g)$loc_com = membership(g$communities)
  V(g)$loc_aff = affiliations(g, "loc_com")
  V(g)$com = V(G)[V(g)$name]$com # From global retweet graph
  V(g)$aff = V(G)[V(g)$name]$aff # From global retweet graph
  
  g %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_min_weight(min_w=1) %>%
    filter_topn_components(topn=1, mode="weak") %>%        
    filter_isolates() 
}


# Get list of users who tweeted a certain day along with community as
# determined from global graph (should be of same mode). 
# Note: users present on specified day, but not in global graph will
# be filtered out!
# Also: will be different from all users present in day graph, as the 
# latter may also include passive retweeted-only users, which will be
# absent in the returned list!
# --------------------------------------------------------------------
day_users_with_com = function(day_str, mode, G=NULL) {
  if (is.null(G)) {
    G = process_ggraph(mode=mode)
  }
  
  users = day_users(day_str, mode)
  if (nrow(users) > 0) {
    users_in_ggraph = which(users$name %in% V(G)$name)
    users = users[users_in_ggraph, ]
    users$com = V(G)[users$name]$com # From global retweet graph
    users$aff = V(G)[users$name]$aff # From global retweet graph
    users
  } else {
    NULL
  }
}

# Number formatting by locale for datatables (DT)
# --------------------------------------------------------------------
library(DT)
formatNumbers = function (table, columns, digits=2, locale="de-DE") 
{
  formatter = function(cols) {
    sprintf("var d = parseFloat(data[%d]); $(this.api().cell(row, %s).node()).html(isNaN(d) ? '' : (+d.toFixed(%d)).toLocaleString('%s'));", 
            cols, cols, digits, locale)
  }
  
  x = table$x
  colnames = base::attr(x, "colnames", exact = TRUE)
  rownames = base::attr(x, "rownames", exact = TRUE)
  x$options$rowCallback = DT:::appendFormatter(x$options$rowCallback, columns, colnames, rownames, formatter)
  table$x = x
  table
}
