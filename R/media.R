source('common.R')

library(xtable)
library(boot)
library(reldist)

community_algo = "louvain"

process_ggraph = function() {
  G = global_graph(ggraph_fnm, total_edgelist_fnm, community_algo) 
  G %>% 
    simplify(edge.attr.comb=list(weight="sum")) %>%
    filter_min_weight(min_w=1) %>%
    filter_topn_components(topn=1, mode="weak") %>%
    filter_attr_not_in(attr="aff", c("animalistas", "unknown")) %>%
    filter_isolates() 
}

g = process_ggraph()

media = get_media(g)
media
dotline_plot(named_to_df(media), "name", "val") + ylab("degree")

# Affiliation of media
med_aff = media_affiliation(g, media)
med_aff

med_grps = media_grp_affiliations(g, media, "aff")
dotline_arrange_nl(med_grps, ylab_str="aff", title="Affiliations for media outlets")

# Same data by party rather than media outlet
grp_med = grp_media_affiliations(g, media, "aff")
dotline_arrange_df(grp_med)

# Gini with bootstrapped confidence intervals ("percentile bootstrap")
boot_errors(gini, grp_med$podemos, rep=1000)
ginis = colwise_boot_errors(gini, grp_med, rep=1000)

ggplot(ginis, aes(x=name, y=v)) + 
  geom_point(size=3) +
  geom_errorbar(aes(ymin=v_min, ymax=v_max), width=0.2) +
  xlab("") + ylab("gini") + ggtitle("Inequality of media affiliation") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=rel(1.25), angle=45, hjust=1, vjust=1), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Above is not taking account of weights (i.e. number of retweets), 
# just count of retweeting accounts
# For tweet count: 1. get neighbourhood of node; 2. get edges from neighbours to node;
# 3. Add weights of edges
