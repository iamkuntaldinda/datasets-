####Network Analysis####

install.packages('igraph')

library(igraph)

g <- graph(c(1,2, 1,3, 2,3, 3,5),n=5)
g
plot(g)

g <- graph.tree(40,4)
plot(g)
plot(g,layout= layout.circle)

plot(g, layout = layout.fruchterman.reingold)
plot(g, layout = layout.graphopt)
plot(g, layout = layout.kamada.kawai)
tkplot(g, layout = layout.kamada.kawai)

l <- layout.kamada.kawai(g)
library(ggplot2)
install.packages('rgl')
require(rgl)
rglplot(g,layout = l)

plot(g,layout = l, vertex.color = "cyan")

g <- graph(c(1,1,1,2,1,3,2,4,3,5,4,5),n = 5)
print.igraph(g,full = TRUE)
plot(g)

##--------------------------------------------------
jets <- read.csv('routes.csv', stringsAsFactors = FALSE, sep = ",", header = TRUE)

head(jets)
flights <- graph.data.frame(jets,directed = TRUE)
print.igraph(flights, full = TRUE)
plot(flights)


## get list of all edges ## use E()
E(flights)


## get list of all vertices ## use V()
V(flights)

## to get count of veertices and edges ------

vcount(flights)
ecount(flights)


#####for no direction#####
flights2 <- as.undirected(flights)
print.igraph(flights2, full = TRUE)
vcount(flights2)
ecount(flights2)
E(flights2)
plot(flights2)

####Graph metrics

average.path.length(flights)

average.path.length(flights2)

diameter(flights)
farthest.nodes(flights)

largest.cliques(flights)[1:2]

transitivity(flights)
degree(flights)

####transitivity tell the relation b/w 3 diff variables...
###degree tells the no of edges for each node...

hist(degree(flights))
shortest.paths(flights)

heatmap(shortest.paths(flights))

V(flights)[degree(flights) >= 30 ]$color <- "green"
V(flights)[degree(flights) < 30 ]$color <- "red"
plot(flights)

plot(flights, edge.width = E(flights)$Time/100)

flights3 <- flights
E(flights3)$weight <- E(flights)$Time
heatmap(shortest.paths(flights3))
shortest.paths(flights3)

###Centrality Measure - 2 types
### Eigen vector 
require(ggplot2)
centrality <- data.frame(Betweenness = betweenness(flights), Eigen = evcent(flights)$vector)
centrality$resid <- lm(Eigen ~ Betweenness, data = centrality)$residuals
head(centrality)
p <- ggplot(centrality, aes(x = Betweenness, y = Eigen,label = rownames(centrality), color = resid, size = abs(resid)))
p + geom_text()












#######diameter calc

diameter(flights)
d <- get.diameter(flights)
V(flights)$size <- 4
E(flights)$color <- "dark grey"
  E(flights)$width <-  1
  ####for nodes along the diameter - we will mention path = d------------
E(flights, path = d)$color <- "red"
E(flights, path = d)$width <- 3

plot(flights, vertex.label=NA)

cores <- graph.coreness(flights)
G2 <- induced.subgraph(flights,as.vector(which(cores > 1)))

V(G2)$size <- 4
plot(G2)

F4 <- flights
F4 <- delete.edges(F4, which(E(F4)$Time < 150))
ecount(F4)
plot(F4)







































































