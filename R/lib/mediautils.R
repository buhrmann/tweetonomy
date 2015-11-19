library(xtable)
library(boot)
library(reldist)

# Media outlets
media_names = c("el_pais", "publico_es", "europapress", "voz_populi", 
                "SextaNocheTV", "eldiarioes", "abc_es", "elperiodico", 
                "elpais_espana", "ElHuffPost", "elmundoes",                  
                "eljueves", "la_informacion")

# Degree of media
get_media = function(graph) {
  m = media_names[which(media_names %in% V(g)$name)]
  sort(sapply(m, function(x) degree(g, x), USE.NAMES=F), decr=T)
}


# Affiliation of media
media_affiliation = function(g, media) {
  med_aff = sapply(names(media), function(x) vertex_attr(g, "aff", x))
  named_to_df(med_aff, alt_names=c("media", "affiliation"))
}

# Returns a list of lists where each list corresponds to a media account and contains
# the proportion of a grp connected to it (retweeting it).
media_grp_affiliations = function(g, media, grp_by) {
  sapply(names(media), function(x) { 
    neighbour_affiliations_prop(g, x, grp_by=grp_by)
  }, simplify=F, USE.NAMES=T)
}

# Returns a data frame where each columns corresponds to a group/affiliation and contains
# the proportion of that group that retweets a particular media account (in each row)
grp_media_affiliations = function(g, media, grp_by="aff", affiliations=party_affiliations) {
  med_affs = media_grp_affiliations(g, media, grp_by)
  grp_names = unique(names(affiliations))
  by_grp = t(vapply(med_affs, FUN=function(X) unlist(X)[grp_names], 
                    FUN.VALUE=numeric(length(grp_names))))
  by_grp = by_grp[, colSums(is.na(by_grp)) < nrow(by_grp)] # Remove parties with no data
  by_grp = data.frame(by_grp)
  by_grp[is.na(by_grp)] = 0
  by_grp
}

# 
# # Gini with bootstrapped confidence intervals ("percentile bootstrap")
# # http://stats.stackexchange.com/questions/21868/gini-coefficient-and-error-bounds
# gini_errors = function(x, rep=1000) {
#   g = gini(x)
#   y = boot(x, gini, rep)
#   q = quantile(y$t, probs=c(0.025, 0.975))
#   data.frame(g=g, g_min=q[[1]], g_max=q[[2]])
# }
# 
# gini_errors(byparty$podemos, rep=1000)
# 
# ginis = apply(byparty, 2, FUN=function(x) gini_errors(x))
# ginis = do.call(rbind, ginis)
# ginis$aff = rownames(ginis)
# ginis$aff = factor(ginis$aff, levels=ginis[order(ginis$g), "aff"])  
# 
# ggplot(ginis, aes(x=aff, y=g)) + 
#   geom_point(size=3) +
#   geom_errorbar(aes(ymin=g_min, ymax=g_max), width=0.2) +
#   xlab("") + ylab("gini") + ggtitle("Inequality of media affiliation") +
#   theme_minimal() +
#   theme(axis.text.x=element_text(size=rel(1.25), angle=45, hjust=1, vjust=1), 
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.y = element_blank())
# 
# 
# # Correlate media consumption inequality with community structure
# # coms_df calculated elsewhere... o_0
# df = merge(coms_df, ginis, by="row.names")
# 
# # Above is not taking account of weights (i.e. number of retweets), 
# # just count of retweeting accounts
# # For tweet count: 1. get neighbourhood of node; 2. get edges from neighbours to node;
# # 3. Add weights of edges
