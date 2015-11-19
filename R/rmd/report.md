

---
title: "Ideological twitter communities"
output: 
  html_document:
    toc: true
    number_section: true
    theme: cosmo
---

This document gathers and illustrates work on a set of tools for analyzing twitter communities and their interaction with hadoop and R.

## Pipeline overview
TODO: explain selection of tweets...

Tweets are stored on a distributed file system (hdfs) as raw json files. A Flume agent based on Twitter4j is used to funnel data received from Twitter's API onto disk. Tweets are arranged in daily folders, with individual files containing a roughly equal number of tweets. Hive provides an SQL-like view on these json files, with Hive tables being partitioned by day also. A number of hql scripts export daily edge-lists for mentions and retweets and other aggregated summaries into local text files. 

A number of different graphs can then be generated from the local data and inspected using the R library. Graphs are distinguished by layer (retweet, mentions or both combined) and version (e.g. one for the period of the catalan elections and one for the general elections). Graphs are also locally cached as R data files, so we don't have to re-create them for each analysis.

## Network level
Each graph layer consists of nodes representing twitter accounts, and edges between those nodes that capture the number of times a user A has retweeted another user B (in the retweet layer R), or how many times A has 'mentioned' B (in the mention layer M). First we will look at some overall statistics for the different graph layers.


In the following sections we'll use tweets from the catalunya election period as an example.

### Descriptive statistics
The following tables provide information about very general statistics of the three graph layers before and after basic preprocessing. The preprocessing consists in simplification of edges (collapsing parallel edges by summing their weights), filtering out edges with weights below some threshold (here 1 is selected, so no filtering), and by filtering out nodes not belonging to the largest (weakly) connected component (this removes nodes which ...). Lastly, stray nodes resulting from the filtering (those not connected to any other) are removed too.




|         |vertex count |edge count |tweets    |max comp size |max coreness |
|:--------|:------------|:----------|:---------|:-------------|:------------|
|retweet  |231.675      |1.138.893  |2.248.769 |227.463       |84           |
|mention  |289.089      |2.424.425  |6.145.886 |286.356       |119          |
|combined |289.089      |2.424.425  |8.394.655 |286.356       |119          |



|         |vertex count |edge count |tweets    |max comp size |max coreness |
|:--------|:------------|:----------|:---------|:-------------|:------------|
|retweet  |227.463      |1.136.365  |2.246.056 |227.463       |84           |
|mention  |286.356      |2.422.694  |6.143.756 |286.356       |119          |
|combined |286.356      |2.422.694  |8.391.559 |286.356       |119          |

Next we can plot the edge weight and node degree distributions of the (preprocessed) retweet layer:

<img src="figure/unnamed-chunk-5-1.png" title="plot of chunk unnamed-chunk-5" alt="plot of chunk unnamed-chunk-5" style="display: block; margin: auto;" />

We can also find various node-centrality (importance) measures. E.g. here are the 5 most central twitter accounts with respect to in-degree (number of retweeters), and page-rank for the retweet layer:


--------------------------------------------------------------------------
 ahorapodemos   Pablo_Iglesias_   Albert_Rivera   ierrejon   CiudadanosCs 
-------------- ----------------- --------------- ---------- --------------
    26.942          24.959           15.087        13.297       12.308    
--------------------------------------------------------------------------

Table: indeg


-------------------------------------------------------------------------
 ahorapodemos   Pablo_Iglesias_   CiudadanosCs   PPopular   marianorajoy 
-------------- ----------------- -------------- ---------- --------------
    0.0539          0.02569         0.01961      0.01743      0.01739    
-------------------------------------------------------------------------

Table: pgrank

Note that the in-degree here corresponds to the number of _users_ having retweeted a particular account, not the number of retweets received (which requires taking into account the weight of each connection).

### Tweet frequency
To get a better feeling for the volume and frequency of tweets, an hql script aggregates the number of tweets per hour. Using these data we can plot the following time series:

TODO...

### Graph
TODO: Filtered down graph view...

## Community level
The goal of the analysis tools is to understand the interaction between different network communities. To this end, in each graph layer communities are identified based on structural network properties. For graphs as large as those explored here few community detection algorithms are sufficiently fast. In the following the "louvain"-method is used (TODO: explain), but others can be substituted too.

As a first step we can identify what level of modularity the community partitioning has achieved. In the case of the "louvain"-method applied to the retweet layer this is . TODO: explain this measure.

Next we check the number and size of communities identified.

* total number of communities: 2082

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

On a log-scale this would likely be close to linear (indicating an exponential distribution of community sizes), as there are a few very large communities and a great number of small communities. In the presented form the communities are of little interest. What we're really interested in is the ideological identity of these communities. We identify them here based on the presence of certain individual accounts in each community. I.e. given a map that assigns certain groups of individuals to their corresponding ideological affiliation, we can represent communities by that affiliation rather than the abstract index in the previous figure. To this end we have simply compiled a list (by hand) of important twitter accounts associated with each political party in Spain. E.g. for the party Podemos, the group of accounts associated with the party itself (@ahorapodemos), their election candidate (@Pablo_Iglesias_), and their spokesperson (@ierrejon) are assigned the affiliation "podemos"; and similarly for all other parties. Having identified these lists of party-related accounts, we can then simply check if each member of a list is also part of a particular twitter community. If that is the case for all members of a list, the community is equated with that party. 

While in theory this doesn't guarantee that all communities, which, remember, are identified solely based on structural network properties, can be uniquely mapped to a party, in practice we found that to be the case. So in the following figure we display again the size of communities, but now with more meaningful identifiers based on political parties:

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

Here, only those communities have been explicitly identified for which we have supplied the manual mapping of party affiliation. The remaining communities are subsumed under the label "unknown". Also, for efficiency reasons the mapping of community to party is only done for the n largest communities, usually 10, as there can be hundreds to thousands of smaller communities. Those accounts not belonging to the 10 biggest communities are filtered out for the rest of the analysis (there are in total 12803 of these). We will also omit the group of "unknown" communities from further analysis, as well as remove accounts that end up isolated as the result of filtering out smaller and unknown communities.

### Community graph
Interaction matrix, 
collapsed graph, 
community colored and filtered graph,


### Comparison of communities
We can compare communities by calculating standard structural graph measures for the their subgraphs, i.e. subgraphs constituted by the nodes belonging to a community and all existing edges between them. The following table lists a number of these measure for each community (TODO: short explanation of each):


|           | indeg ineq| outdeg ineq| pgrank ineq| indeg| outdeg| eigen| max core| density| reciprocity| transitivity|  size| weight| num edges| avg weight|
|:----------|----------:|-----------:|-----------:|-----:|------:|-----:|--------:|-------:|-----------:|------------:|-----:|------:|---------:|----------:|
|podemos    |       0.98|        0.67|        0.81|  0.35|   0.02|  1.00|       58|       0|        0.03|         0.00| 66712| 578032|    276267|       2.09|
|pp         |       0.96|        0.73|        0.76|  0.29|   0.02|  1.00|       65|       0|        0.05|         0.02| 31052| 394737|    167950|       2.35|
|catalunya  |       0.97|        0.68|        0.69|  0.11|   0.01|  1.00|       40|       0|        0.01|         0.02| 27325| 146927|    124079|       1.18|
|iu         |       0.98|        0.51|        0.73|  0.21|   0.01|  1.00|       20|       0|        0.03|         0.00| 26169|  88912|     57309|       1.55|
|ciudadanos |       0.98|        0.73|        0.80|  0.48|   0.03|  1.00|       83|       0|        0.05|         0.02| 25799| 401872|    147303|       2.73|
|psoe       |       0.96|        0.72|        0.76|  0.38|   0.04|  1.00|       67|       0|        0.07|         0.03| 20406| 279263|     93591|       2.98|
|upyd       |       0.93|        0.62|        0.68|  0.15|   0.06|  0.99|       23|       0|        0.07|         0.09|  3198|  13581|      9155|       1.48|
|vox        |       0.90|        0.74|        0.72|  0.31|   0.06|  0.99|       32|       0|        0.11|         0.18|  2181|  22613|     10272|       2.20|

Next we can pick out interesting community measures by identifying, for example, pairs with greatest spread (i.e. those best separating the communities, as measured by standard deviation), but little correlation (not measuring "the same thing", small absolute correlation).


|Measure 1    |Measure 2    | Correlation|
|:------------|:------------|-----------:|
|avg_weight   |density      |      -0.012|
|avg_weight   |eigen        |       0.015|
|avg_weight   |transitivity |      -0.015|
|size         |outdeg_ineq  |      -0.031|
|density      |indeg        |      -0.048|
|reciprocity  |max_core     |      -0.051|
|avg_weight   |indeg_ineq   |       0.069|
|avg_weight   |size         |       0.069|
|transitivity |indeg        |      -0.075|
|eigen        |indeg        |       0.113|

<!-- html table generated in R 3.1.3 by xtable 1.7-4 package -->
<!-- Tue Oct 27 18:26:51 2015 -->
<table 0>
<caption align="bottom"> 10 measurement pairs with lowest absolute correlation </caption>
<tr> <th> Var1 </th> <th> Var2 </th> <th> Cor </th>  </tr>
  <tr> <td> avg_weight </td> <td> density </td> <td align="right"> -0.01 </td> </tr>
  <tr> <td> avg_weight </td> <td> eigen </td> <td align="right"> 0.02 </td> </tr>
  <tr> <td> avg_weight </td> <td> transitivity </td> <td align="right"> -0.02 </td> </tr>
  <tr> <td> size </td> <td> outdeg_ineq </td> <td align="right"> -0.03 </td> </tr>
  <tr> <td> density </td> <td> indeg </td> <td align="right"> -0.05 </td> </tr>
  <tr> <td> reciprocity </td> <td> max_core </td> <td align="right"> -0.05 </td> </tr>
  <tr> <td> avg_weight </td> <td> indeg_ineq </td> <td align="right"> 0.07 </td> </tr>
  <tr> <td> avg_weight </td> <td> size </td> <td align="right"> 0.07 </td> </tr>
  <tr> <td> transitivity </td> <td> indeg </td> <td align="right"> -0.08 </td> </tr>
  <tr> <td> eigen </td> <td> indeg </td> <td align="right"> 0.11 </td> </tr>
   </table>


| indeg ineq| outdeg ineq| pgrank ineq| indeg| outdeg| eigen| max core| density| reciprocity| transitivity|  size| weight| num edges| avg weight|
|----------:|-----------:|-----------:|-----:|------:|-----:|--------:|-------:|-----------:|------------:|-----:|------:|---------:|----------:|
|       0.03|       0.116|       0.064| 0.437|  0.608| 0.004|    0.474|   1.448|       0.562|        1.335| 0.789|  0.854|     0.806|      0.303|

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The two plots indicate parties along pairs of measure (from the tables above), that show low correlation but large spread. Not however, that some measures may be related to the size of the community, and since sizes are substantially different, might not be easily comparable.

<!---acts = important_actors_by_com(g, topn=10, centrality=centr, grp_by="aff")
centr = "indeg"
plot_important_actors(acts, centr)-->

## Hashtag level

