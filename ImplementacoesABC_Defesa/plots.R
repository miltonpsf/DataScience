A<-asIgraph(grafosSimulados[[1]][[4]][1][[1]])
B<-asIgraph(grafosSimulados[[3]][[4]][1][[1]])
C<-asIgraph(grafosSimulados[[6]][[4]][1][[1]])
D<-asIgraph(grafosSimulados[[9]][[4]][1][[1]])
plot(A, layout=layout.fruchterman.reingold.grid, vertex.color="green")
plot(B, layout=layout.reingold.tilford, vertex.color="green")#o importante
plot(C, layout=layout.reingold.tilford, vertex.color="green")#o importante
plot(D, layout=layout.drl , vertex.color="green")
plot(degree.distribution(A, cumulative = FALSE))
plot(degree.distribution(D, cumulative = FALSE))
par(mfrow=c(2,2))
hist(degree(as.network.matrix(get.adjacency(A))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="A")
hist(degree(as.network.matrix(get.adjacency(B))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="B")
hist(degree(as.network.matrix(get.adjacency(C))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="C")
hist(degree(as.network.matrix(get.adjacency(D))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="D")

AA <- get.adjacency(A, sparse=FALSE)
gA <- network::as.network.matrix(AA)

sna::gplot.target(gA, degree(gA), main="Degree A",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
sna::gplot.target(gA,  betweenness(gA), main=" Betweenness A",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
coresA <- graph.coreness(A)
sna::gplot.target(gA, coresA,main=" Corenness A",
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=coresA, edge.col="darkgray")

AB <- get.adjacency(B, sparse=FALSE)
gB <- network::as.network.matrix(AB)

sna::gplot.target(gB, degree(gB), main="Degree B",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
sna::gplot.target(gB,  betweenness(gB), main=" Betweenness B",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
coresB <- graph.coreness(B)
sna::gplot.target(gB, cores, circ.lab = FALSE,main=" Corenness B",
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=cores, edge.col="darkgray")

AC <- get.adjacency(C, sparse=FALSE)
gC <- network::as.network.matrix(AC)

sna::gplot.target(gC, degree(gC), main="Degree C",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
sna::gplot.target(gC,  betweenness(gC), main=" Betweenness C",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
coresC <- graph.coreness(C)
sna::gplot.target(gC, coresC, circ.lab = FALSE,main=" Corenness C",
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=coresC, edge.col="darkgray")

AD <- get.adjacency(D, sparse=FALSE)
gD <- network::as.network.matrix(AD)

sna::gplot.target(gD, degree(gD), main="Degree D",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
sna::gplot.target(gD,  betweenness(gD), main=" Betweenness D",
                  circ.lab = FALSE, circ.col="skyblue",
                  usearrows = FALSE,
                  vertex.col="blue",
                  edge.col="darkgray")
coresD <- graph.coreness(D)
sna::gplot.target(gD, coresD, circ.lab = FALSE,main=" Corenness D",
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=coresD, edge.col="darkgray")