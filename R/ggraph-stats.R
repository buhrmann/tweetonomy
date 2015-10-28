# --------------------------------------------------------------------------------
#
# --------------------------------------------------------------------------------
source('common.R')
source('mediautils.R')
community_algo = "louvain"
topn_coms = 10


# Get graph
# --------------------------------------------------------------------------------
r = process_ggraph(mode="R")
m = process_ggraph(mode="M")

# Communities most often categorizes as "unknown"
V(g)$degree = degree(g)
unk = vertex_attribs(g, which(V(g)$aff=="unknown"), sort_by="degree") %>% 
  group_by(com) %>% 
  summarize(cnt=n()) %>% 
  arrange(desc(cnt))

head(vertex_attribs(g, which(V(g)$com==unk$com[1]), sort_by="degree"), 10)
head(vertex_attribs(g, which(V(g)$com==unk$com[2]), sort_by="degree"), 10)

g = filter_attr_not_in(g, attr="aff", c("animalistas", "unknown"))

# Assign colors
# pcolors = as.list(group_sizes(g, "aff"))
# pcolors$podemos = "#612F62"
# pcolors$pp = "#1AA1DB"
# pcolors$iu = "#DA0B31"
# pcolors$ciudadanos = "#E96A32"
# pcolors$catalunya = "#d3d323"
# pcolors$psoe = "#DF1223"
# pcolors$upyd = "#E0147B"
# pcolors$vox = "#6BBD1F"
# pcolors$unknown = "#cccccc"
# pcolors$prensa = "#666666"

# Basic stats
# --------------------------------------------------------------------------------
vcount(g)
ecount(g)
component_sizes(g, "weak")
component_sizes(g, "strong")
kcore_dist(g)
modularity(g$communities)

# Weight distribution
weight_dist(g)
barplot(weight_dist(g), log="xy")
ggplot(plyr::count(E(g)$weight), aes(x=x, y=freq)) + geom_point(stat='identity') + scale_y_log10() + scale_x_log10() + theme_minimal()

# Degree distribution
barplot(table(degree(g)), log="xy")
ggplot(plyr::count(degree(g)), aes(x=x, y=freq)) + geom_point(stat='identity') + scale_y_log10() + scale_x_log10() + theme_minimal()

# Community size distribution
plot_group_sizes(g) + scale_color_manual(values=pcolors)


# Identify important actors by centrality
centr = "indeg"
acts = important_actors_by_com(g, topn=10, centrality=centr, grp_by="aff")
plot_important_actors(acts, centr)


# Communities' structural properties
# --------------------------------------------------------------------------------
# Show within-group inequality of centralities (e.g. in degree inequality)
# Small world, e.g.,  means short avg path lengh and high clustering (transitivity)
# All the following functions return measures in order of group size for consistency
coms_df = com_centralizations(g, grp_by="aff", verbose=T, except=c())
coms_df$name = rownames(coms_df)

p1 = ggplot(coms_df, aes(x=indeg_centr, y=avg_path_len, label=name, color=name)) + 
  geom_point(size=3) + geom_text(vjust=1.5, hjust=1) +
  scale_x_continuous(expand=c(0.1, 0.1)) +
  scale_y_continuous(expand=c(0.1, 0.1)) +
  scale_color_manual(values=pcolors) +
  theme_minimal() + theme(legend.position="none") 

p2 = ggplot(coms_df, aes(x=indeg_centr, y=max_coreness, label=name, colour=name)) + 
  geom_point(size=3) + geom_text(vjust=1.5, hjust=1) +
  scale_x_continuous(expand=c(0.1, 0.1)) +
  scale_y_continuous(expand=c(0.1, 0.1)) +
  scale_color_manual(values=pcolors) +
  theme_minimal() + theme(legend.position="none") 

grid.arrange(p1, p2, ncol=2)

# Pairwise correlations between group measures (and 6 biggest groups)
pairs(coms_df[1:6,-17])
corr = round(cor(coms_df[1:6,-17]), 2)
plot_interaction_matrix(corr)
# Which pairwise measures have the lowest correlation?
corr_molten = na.omit(melt(corr, value.name="Cor"))
corr_molten = corr_molten[order(abs(corr_molten$Cor)), ]

# Standard deviation of scaled measures: which measure spreads the groups out the most?
coms_sc = apply(coms_df[,-17], 2, FUN=function(x) x/max(x))
apply(coms_sc, 2, sd)


ggplot(coms_df[1:6,], aes(x=pr_ineq, y=density, label=name, colour=name)) + 
  geom_point(size=3) + geom_text(vjust=1.5, hjust=1) +
  scale_color_manual(values=pcolors) +
  theme_minimal() + theme(legend.position="none") 


# Correlate grp media consumption with structural properties
# --------------------------------------------------------------------------------
media = get_media(g)
media_affiliation(g, media)
grp_med = grp_media_affiliations(g, media, "aff")     # Each grp's prop. of retweeters per media
ginis = colwise_boot_errors(gini, grp_med, rep=1000)  # Inequality of proportions with error bars
df = merge(coms_df, ginis, by="name")

p1 = ggplot(df, aes(x=deg_ineq, y=v, label=name, colour=name)) + 
  geom_point(size=3) + geom_text(vjust=1.5, hjust=1) +
  #scale_x_continuous(expand=c(0.05, 0.05)) +
  #scale_y_continuous(expand=c(0.05, 0.05)) +
  scale_color_manual(values=pcolors) +
  theme_minimal() + theme(legend.position="none") +
  xlab("gini(in-degree)") + ylab("gini(media-consumption)")

p2 = ggplot(df, aes(x=indeg_centr, y=v, label=name, colour=name)) + 
  geom_point(size=3) + geom_text(vjust=1.5, hjust=1) +
  scale_x_continuous(expand=c(0.1, 0.0)) +
#   scale_y_continuous(expand=c(0.1, 0.1)) +
  scale_color_manual(values=pcolors) +
  theme_minimal() + theme(legend.position="none") +
  xlab("centrality(in-degree)") + ylab("")

grid.arrange(p1, p2, ncol=2)


# Interaction between communities
# --------------------------------------------------------------------------------
# Interaction matrix
im = round(interaction_matrix(g, grp_by="aff", scale=T), 2)
plot_interaction_matrix(im) + xlab("retweeted") + ylab("retweeter")

# Contracted plot
cols = adjust_hsv(pcolors, "s", 0.6)
cols = adjust_hsv(cols, "v", 1.4)
plot_contracted_by(g, "aff", cols)


# "Complete" graph
# --------------------------------------------------------------------------------
pg = g %>% 
  filter_min_weight(5) %>%
  filter_min_degree(3) %>%
  filter_isolates()

vcount(pg)
ecount(pg)

vcols = as.character(cols[V(pg)$aff])
V(pg)$color = vcols
g_plot(pg, layout=with_fr(), colorAttr=NULL)
