# =========================================================
# Social Network Analysis with R | Examples
# (Dr. Bharatendra Rai's tutorial - matches video workflow)
# =========================================================

# ---- Load Package ----
library(igraph)

# ---- 00:40 Social Network example (simple toy graphs) ----
g <- graph(c(1,2, 2,3, 3,4, 4,1),
           directed = F,
           n = 7)
plot(g)

g1 <- graph(c("Amy","Ram", "Ram","Li", "Li","Amy",
              "Amy","Li", "Kate","Li"),
            directed = T)
plot(g1)

# ---- 06:33 Network measures ----
degree(g1, mode = 'all')
degree(g1, mode = 'in')
degree(g1, mode = 'out')

diameter(g1, directed = F, weights = NA)

edge_density(g1, loops = F)
ecount(g1) / (vcount(g1) * (vcount(g1) - 1))

reciprocity(g1)

closeness(g1, mode = 'all', weights = NA)

betweenness(g1, directed = T, weights = NA)
edge_betweenness(g1, directed = T, weights = NA)

# ---- 12:07 Read data file ----
data <- read.csv('https://raw.githubusercontent.com/bkrai/R-files-from-YouTube/main/networkdata.csv', header = T)
y <- data.frame(data$first, data$second)

# ---- 14:42 Create network ----
net <- graph.data.frame(y, directed = T)
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)

# ---- 16:20 Histogram of node degree ----
hist(V(net)$degree,
     col = 'lightblue',
     main = 'Histogram of Node Degree',
     xlab = 'Degree')

# ---- 17:13 Network diagram ----
plot(net)

# ---- 18:56 Highlighting degree and changing layouts ----
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree * 0.4,
     edge.arrow.size = 0.1,
     layout = layout.fruchterman.reingold)

# ---- 21:01 Hubs and Authorities ----
hs <- hub_score(net)$vector
as <- authority.score(net)$vector

par(mfrow = c(1,2))
set.seed(123)
plot(net,
     vertex.size = hs * 30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)

plot(net,
     vertex.size = as * 30,
     main = 'Authorities',
     vertex.color = rainbow(52),
     edge.arrow.size = 0.1,
     layout = layout.kamada.kawai)
par(mfrow = c(1,1))

# ---- 24:43 Community detection ----
net <- graph.data.frame(y, directed = F)
cnet <- cluster_edge_betweenness(net)
plot(cnet, net)