####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+####+

# Marc d. Paradis
# R 3.5.1 for Windows
# RStudio 1.1.456

# Based on code by Chris Hane

require(RColorBrewer)

require(data.table)

require(igraph)

# http://networkdata.ics.uci.edu/browse.php?tags=Copurchase%20Networks&directed=&dynamic=&weighted=&sortby=&nodeatts=&edgeatts=
# Books about US politics
# Compiled by Valdis Krebs
# 
# Nodes represent books about US politics sold by the online bookseller
# Amazon.com.  Edges represent frequent co-purchasing of books by the same
# buyers, as indicated by the 'customers who bought this book also bought
# these other books' feature on Amazon.
# 
# Nodes have been given values 'l', 'n', or 'c' to indicate whether they are
# 'liberal', 'neutral', or 'conservative'.  These alignments were assigned
# separately by Mark Newman based on a reading of the descriptions and
# reviews of the books posted on Amazon.
# 
# These data should be cited as V. Krebs, unpublished,
# http://www.orgnet.com/.

wrkng.path <- 
  'C:/Users/mparadis/Desktop/DSU/Immersion College/02 MLM/05. Unsupervised Clustering/5.4 Graph Based Clustering/' 
#note you have to change backslashes (\) to forward slashes (/)

pb <- 
  read_graph(
    file = paste(
      wrkng.path,
      'polbooks.gml',
      sep = ''
      ), 
    'gml'
    )

graph_attr_names(pb)

edge_attr_names(pb)

vertex_attr_names(pb)

V(pb)$value  # a supervised labeling of the book content

V(pb)$label

edge_density(
  graph = pb, 
  loops = F
  ) # fraction of edges present relative to total possible edges

transitivity(
 graph = pb, 
  type = 'global' # 'local'
  ) # fraction of triangles (completely connected 3 nodes) / all triangles

diameter(pb) # longest path

deg <- 
  degree(
    pb, 
    mode = 'all'
    )

V(pb)$label[deg > 20] # Vertices of a graph where degree > 20

hist(deg)

hist(log(deg))

deg.dist <- 
  degree_distribution(
    pb, 
    cumulative = T, 
    mode = 'all'
    )

plot(
  x = 0:max(deg), 
  y = 1 - deg.dist, 
  pch = 19, 
  cex = 1.2, 
  col = 'orange',
  xlab = 'Degree', 
  ylab = 'Cumulative Frequency'
  )

plot(
  pb, 
  layout = layout_in_circle(pb),
  vertex.size = deg,
  vertex.label.cex = deg * 0.05
  )

plot(
  pb, 
  layout = layout_randomly,
  vertex.size = 10,
  vertex.label.cex = 0.7
  )

# Fruchterman-Rheingold is a physical model where edges are springs and layout
# seeks minimum energy
l <- 
  layout_with_fr(pb)  # this is non-deterministic and will vary from call to 
#                     # call

plot(
  pb, 
  layout = l,
  vertex.size = 10,
  vertex.label.cex = 0.7
  )

# find cliques
hist(
  sapply(
    cliques(pb), 
    length
    )
  ) # clique sizes

largest_cliques(pb) # cliques with max number of nodes

vcol <- 
  rep(
    'grey80', 
    vcount(pb)
    )

vcol[
  unlist(
    largest_cliques(pb)
    )
  ] <- 'gold'

plot(pb,
     layout = layout_with_kk(pb),
     vertex.label = V(pb)$name,
     vertex.color = vcol,
     vertex.size = 5,
     vertex.label.cex = 0.7
     )

kc <- 
  coreness(
    pb, 
    mode = 'all'
    )

colrs <- 
  adjustcolor(
    RColorBrewer::brewer.pal(
      6, 
      'Set1'
      ), 
    alpha = 0.6
    )

plot(
  pb, 
  layout = layout_with_kk(pb),
  vertex.size = kc * 3,
  vertex.label = kc, 
  vertex.color = colrs[kc]
  )

# get the leading Eigen value clusters
cls_eigen <- 
  cluster_leading_eigen(pb)

table(
  membership(cls_eigen)
  )

# improve layout by adding weights for edges crossing the clusters
weights <- 
  ifelse(
    crossing(
      cls_eigen, 
      pb
      ), 
    1, 
    10
    )

layout <- 
  layout_with_fr(
    pb, 
    weights = weights
    )

plot(
  pb, 
  layout = layout,
  vertex.size = 5,
  vertex.label = membership(cls_eigen),
  vertex.color = colrs[
    membership(cls_eigen)
    ]
  )

## or plot directly
plot(
  cls_eigen, 
  y = pb,
  layout = layout,
  vertex.size = 3,
  vertex.label.cex = 0.5
  )

cls_louvain <- 
  cluster_louvain(pb)

table(
  membership(cls_louvain)
  )

# improve layout by adding weights for edges crossing the clusters
weights <- 
  ifelse(
    crossing(
      cls_louvain, 
      pb
      ), 
    1, 
    10)

layout <- 
  layout_with_fr(
    pb, 
    weights = weights
    )

plot(
  pb, 
  layout = layout,
  vertex.size = 5,
  vertex.label = membership(cls_louvain),
  vertex.color = colrs[
    membership(cls_louvain)
    ]
  )

## or plot directly
plot(
  cls_louvain, 
  y = pb,
  layout = layout,
  vertex.size = 3,
  vertex.label.cex = 0.5
  )

cls_wt <- 
  cluster_walktrap(
    pb,
    steps = 4
    )

table(
  membership(cls_wt)
  )

# improve layout by adding weights for edges crossing the clusters
weights <- 
  ifelse(
    crossing(
      cls_wt, 
      pb
      ),  
    1, 
    10
    )

layout <- 
  layout_with_fr(
    pb, 
    weights = weights
    )

plot(
  pb, 
  layout = layout,
  vertex.size = 5,
  vertex.label = membership(cls_wt),
  vertex.color = colrs[
    membership(cls_wt)
    ]
  )

## or plot directly
plot(
  cls_wt, 
  y = pb,
  layout = layout,
  vertex.size = 3,
  vertex.label.cex = 0.5
  )


modularity(cls_wt)

modularity(cls_eigen)

modularity(cls_louvain)

compare(
  cls_eigen, 
  cls_louvain, 
  method = 'vi'
  )

compare(
  cls_eigen, 
  cls_wt, 
  method = 'vi'
  )

compare(
  cls_louvain, 
  cls_wt, 
  method = 'vi'
  )

### use clv to compare clusters
require(clv)

dd <- 
  distances(
    pb, 
    mode = 'all'
    ) #all shortest paths in V*V matrix

cls.wt.diss <- 
  cls.scatt.diss.mx(
    dd, 
    as.integer(
      membership(cls_wt)
      )
    )

cls.louvain.diss <- 
  cls.scatt.diss.mx(
    dd, 
    as.integer(
      membership(cls_louvain)
      )
    )

clv.Dunn(
  cls.wt.diss,      
  'average', 
  'average'
  )

clv.Dunn(
  cls.louvain.diss, 
  'average', 
  'average'
  )

rm(
  list = ls()
  )

# END