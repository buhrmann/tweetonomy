library(igraph)
library(tools)

# Working directory
root_dir = "~/Code/tweetonomy/"
setwd(paste0(root_dir, "R"))
source('utils.R')
source('fileutils.R')

# Define paths
tblnm = "parties"
data_dir = paste0(root_dir, "data/", tblnm, "/")
ggraph_fnm = paste0(data_dir, "r-cache/global-graph.Rdata")
total_edgelist_fnm = paste0(data_dir, "retweet-edges/all-retweet-edges.txt")