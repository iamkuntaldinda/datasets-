library("igraph") # load library

#Create a directed network graph of airline routes using routes.dat
airline_routes <- read.csv("routes.dat", header=FALSE)
colnames(airline_routes)
colnames(airline_routes) <- c("Airline", "Airline ID", "Source Airport","Source Airport ID","Destination Airport","Destination Airport ID","Stops","Equipment")
head(airline_routes)

AirlineNW <- graph.edgelist(as.matrix(airline_routes[,c(3,5)]), directed=FALSE)
indegree <- degree(AirlineNW,mode="in")
outdegree <- degree(AirlineNW,mode="out")
closeness_in <- closeness(AirlineNW, mode="in",normalized = TRUE)
btwn <- betweenness(AirlineNW,normalized = TRUE)
eigenv <- eigen_centrality(AirlineNW, directed = TRUE, scale = FALSE, weights = NULL)
centralities <- cbind(indegree, outdegree, closeness_in, btwn,x)

colnames(centralities) <- c("inDegree","outDegree","closenessIn","betweenness","eigen")

x <- (eigenv$vector)


head(centralities)
dim(centralities)

degree(AirlineNW, v = V(AirlineNW), mode = c("out"),
       loops = TRUE, normalized = TRUE)
degree_distribution(AirlineNW, cumulative = FALSE)



#vertices of edges
V(AirlineNW)
 
##

A <- leading.eigenvector.community(AirlineNW)
membership(A)
plot(A,AirlineNW)

x <- A$eigenvectors


length(x)


# Which is one of the most important airport in the world in terms of how many other important airports connect to them?
eigenv <- eigen_centrality(AirlineNW, directed = TRUE, scale = FALSE, weights = NULL)
eigenv$vector
max(eigenv$vector)
index <- which(eigenv$vector == max(eigenv$vector))
eigenv$vector[index]
eigenv$options


A <- leading.eigenvector.community(AirlineNW)

length(V(AirlineNW))
length(A$membership)
A$modularity
A$eigenvalues

###
sub1 <- subgraph(AirlineNW, which(membership(A) == 19))
sub1
vcount(sub1)
ecount(sub1)


# Which airport has most flights coming in, and how many?
indegree <- degree(sub1,mode="in")
max(indegree)
index <- which(indegree == max(indegree))
indegree[index]


# Which airport has most flights going out of, and how many?
outdegree <- degree(sub1,mode="out")
max(outdegree)
index <- which(outdegree == max(outdegree))
outdegree[index]

closeness_in <- closeness(sub1, mode="in",normalized = TRUE)
max(closeness_in)
index <- which(closeness_in == max(closeness_in))
closeness_in[index]

btwn <- betweenness(sub1,normalized = TRUE)
max(btwn)
index <- which(btwn == max(btwn))
btwn[index]

eigenv <- eigen_centrality(sub1, directed = TRUE, scale = FALSE, weights = NULL)
eigenv$vector
max(eigenv$vector)
index <- which(eigenv$vector == max(eigenv$vector))
eigenv$vector[index]


plot(sub1,vertex.size=3, layout=layout.kamada.kawai,edge.width=0.5,edge.color="black")
sizes(A)


############2nd community
which(membership(A) == 3)
sub2 <- subgraph(AirlineNW, which(membership(A) == 3))
sub2
vcount(sub2)
ecount(sub1)
plot(sub2,vertex.size=3, layout=layout.kamada.kawai,edge.width=0.5,edge.color="black")



############3rd community
which(membership(A) == 13)
sub2 <- subgraph(AirlineNW, which(membership(A) == 13))
sub2
vcount(sub2)
ecount(sub1)
plot(sub2,vertex.size=3, layout=layout.kamada.kawai,edge.width=0.5,edge.color="black")



############4th community
which(membership(A) == 18)
sub2 <- subgraph(AirlineNW, which(membership(A) == 18))
sub2
vcount(sub2)
ecount(sub1)
plot(sub2,vertex.size=3, layout=layout.kamada.kawai,edge.width=0.5,edge.color="black")



############2nd community
which(membership(A) == 1)
sub2 <- subgraph(AirlineNW, which(membership(A) == 1))
sub2
vcount(sub2)
ecount(sub1)
plot(sub2,vertex.size=3, layout=layout.kamada.kawai,edge.width=0.5,edge.color="black")


####clustering
normalized_data <- scale(centralities) ## normalize the columns
fit <- kmeans(normalized_data, centers=25, iter.max=10, nstart=4)
## centers: either the number of clusters, or a set of initial (distinct) cluster centres. If a number, a random set of (distinct) rows in x is chosen as the initial centres.
## iter.max: the maxi1mum number of iterations allowed.
## nstart: if centers is a number, how many random sets should be chosen.
fit

fit$cluster      # A vector of integers (from 1:k) indicating the cluster to which each point is allocated.
fit$centers      # A matrix of cluster centres.
fit$totss        # The total sum of squares.
fit$withinss     # Vector of within-cluster sum of squares, one component per cluster.
fit$tot.withinss # Total within-cluster sum of squares, i.e. sum(withinss).
fit$betweenss    # The between-cluster sum of squares, i.e. totss-tot.withinss.
fit$size         # The number of points in each cluster.
fit$iter         # The number of (outer) iterations.
#View(fit)
fit$ifault
## Determine number of clusters
Cluster_Variability <- matrix(nrow=30, ncol=1)
for (i in 1:30) Cluster_Variability[i] <- kmeans(normalized_data,centers=i, nstart=4)$tot.withinss
plot(1:30, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot


