library(igraph)
library(tools)
library(utils)

# Working directory
source('fileutils.R')
source('graphutils.R')
source('plotutils.R')
source('mediautils.R')


# Pretty print an integer with thousands separator
# ------------------------------------------------------------------------------------------
pp_int = function(i) {
  formatC(i, big.mark=".", format="f", drop0trailing=T)
}

pprint_date = function(d) {
  format(d, format="%d-%m-%Y")
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
  
process_ggraph = function(layer="C", version="A") {
  G = global_graph(layer, version, community_algo) 
  preprocess_graph(G)
}


# Get day graph with affiliations from global graph
# --------------------------------------------------------------------
day_graph = function(day_str, layer, version, com_algo, G=NULL) {
  if (is.null(G)) {
    G = process_ggraph(layer=layer, version=version)
  }
  
  if (layer == "R") {
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
day_users_with_com = function(day_str, layer, version, G=NULL) {
  if (is.null(G)) {
    G = process_ggraph(layer=layer, version=version)
  }
  
  users = day_users(day_str, layer)
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


# improved list of objects
# --------------------------------------------------------------------
.ls.objects <- function (pos = 1, pattern, order.by, decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(format(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
