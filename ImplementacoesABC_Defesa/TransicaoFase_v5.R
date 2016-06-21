# Gera grafo igraph
# Converte igraph para network
# Simula 

rm(list=ls())
library(igraph)
library(network)
library(Bergm)
library(mixer)
library(intergraph)
library(sna)
nGrafosSim <- 2

SimulaGrafos <- function (pNVertices, ptheta1, ptheta2, pgNetworkBase) {
  # gIgraph <- erdos.renyi.game(nVertices, prob, type = "gnp")
  # gAdjMatrix <- as.matrix(get.adjacency(gIgraph))
  # gNetworkBase <- network(gAdjMatrix, directed=FALSE)
#   pNVertices  <- 50
#   ptheta1     <- -0.8*2
#   ptheta2     <- 0.5*6/50
#   pgNetworkBase <- network(pNVertices,density = 0.5, directed = FALSE)
  gErgmSim <- simulate(pgNetworkBase ~ edges + triangle, 
                      coef=c(ptheta1,ptheta2),nsim=nGrafosSim,
                      control=control.simulate(MCMC.burnin=1000, MCMC.interval=100),
                      statsonly=FALSE,
                      sequential = FALSE)
  
  nArestas <- attr(gErgmSim,"stats")[,1][1]
  nTriangulos <- attr(gErgmSim,"stats")[,2][1] 
  densidades <- network.density(gErgmSim[[1]])
  return(list(nArestas, nTriangulos, densidades, gErgmSim))
}

nVertices <- 50
theta1Aux <- -0.8
theta1    <- theta1Aux*2
prob      <- 0.2

gNetworkBase <- network(nVertices,density = prob, directed = FALSE)
table(degree(gNetworkBase, cmode = "indegree"))
summary(gNetworkBase ~ edges + triangle + kstar(2) + kstar(3) + cycle(3) + cycle(4))

theta2Aux <- seq(0.5,1.5, by=0.1)
theta2    <- theta2Aux*6/50
nArestasSim      <- matrix(nrow=length(theta2),ncol=1)
nTriangulosSim   <- matrix(nrow=length(theta2),ncol=1)
densidadeSim     <- matrix(nrow=length(theta2),ncol=1)
grafosSimulados  <- list()  
for (i in 1:length(theta2)) {
  grafosSimulados[[i]]       <- SimulaGrafos(pNVertices = nVertices, 
                                  ptheta1 = theta1, 
                                  ptheta2 = theta2[i], 
                                  gNetworkBase)
  
  nArestasSim[i]      <- grafosSimulados[[i]][[1]]
  nTriangulosSim[i]   <- grafosSimulados[[i]][[2]]
  densidadeSim[i] <- as.numeric(grafosSimulados[[i]][[3]])
  
}  
class(grafosSimulados[[1]][[4]][1][[1]])
gErgmSim <- grafosSimulados[[1]][[4]][1][[1]]
table(degree(gErgmSim, cmode = "indegree"))
summary(gErgmSim ~ edges + triangle + kstar(2) + kstar(3) + cycle(3) + cycle(4))

class(gErgmSim)
str(grafosSimulados[[1]][[4]][1][[1]])

par(mfrow=c(1,3))
plot(nArestasSim, nTriangulosSim, xlab = "edges", ylab = "triangles", main = paste("Theta1 : ", theta1, "\nProb : ", prob))
plot(densidadeSim, main = paste("Densidade. \nProb : ", prob))
auxBoxPlot <- as.data.frame(densidadeSim)
boxplot(auxBoxPlot)

####################
# calcula Laplaciano
####################
centralidade   <- list()
transitividade <- list()
cv             <- list()
gIgraphSim     <- list()
gLaplac        <- list()
eigenLaplac    <- list()
for (i in 1:length(theta2) ) {
  gErgmSim <- grafosSimulados[[i]][[4]][1][[1]]
  gIgraphSim[[i]] <- asIgraph(gErgmSim)
  gNetworkSim[[i]] <- asNetwork(gErgmSim)
  gLaplac[[i]] <- graph.laplacian(gIgraphSim[[i]])
  eigenLaplac[[i]] <- eigen(gLaplac[[i]], only.values = TRUE)
  
  #Centralidade
  centralidade[[i]] <- igraph::closeness(gIgraphSim[[i]], V(gIgraphSim[[i]]))
  #Densidade de triangulos
  transitividade[[i]] <- igraph::transitivity(gIgraphSim[[i]], type="undirected")
  cv[[i]] <- sd(centralidade[[i]])/mean(centralidade[[i]])
}

par(mfrow=c(1,2))
plot(as.matrix(transitividade), xlab = "vertices", ylab = "transitividade", main = paste("Theta1 : ", theta1,"\n Transitividade"))
plot(as.matrix(cv), ylab = "Coeficiente de variacao", main = paste("Theta1 : ", theta1,"\n Coeficiente de variacao"))
par(mfrow=c(1,3))
for (i in 1:length(theta2)) {
  plot(as.matrix(centralidade[[i]]), xlab = "vertices", ylab = "Centralidade", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\n Centralidade Grafo",i)) 
}

#####################
# Calcula autovalores
#####################
par(mfrow=c(1,3))
for (i in 1:length(theta2)) {
   plot(eigenLaplac[[i]]$values, xlab = "vertices", ylab = "Autovalores", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nAutovalores Grafo : ", i))
}


#########################################
# Identifica quantos grupos foram gerados 
#########################################
limSupEigenValues = 0.5
numGrupo     <- matrix(nrow=length(theta2),ncol=1)
for (i in 1:length(theta2)) {
  numGrupo[i] <- length(eigenLaplac[[i]]$values[which(eigenLaplac[[i]]$values < limSupEigenValues,)])
}
####################################
# Gera uma mistura de grafos (mixer)
####################################
gAdjMatrixSim <- as.matrix(get.adjacency(gIgraphSim[[6]], attr=NULL, edges=FALSE, names=FALSE))
gMixer <- mixer(gAdjMatrixSim, qmin = numGrupo[6], qmax = numGrupo[1], method="bayesian")

####################################
# Adrian
####################################
A<-asIgraph(grafosSimulados[[1]][[4]][1][[1]])   #0.5
#B<-asIgraph(grafosSimulados[[3]][[4]][1][[1]])   #0.7 = Adrian
B<-asIgraph(grafosSimulados[[6]][[4]][1][[1]])   #1
#C<-asIgraph(grafosSimulados[[6]][[4]][1][[1]])   #1 - Adrian
C<-asIgraph(grafosSimulados[[7]][[4]][1][[1]])   #1.1
D<-asIgraph(grafosSimulados[[9]][[4]][1][[1]])   #1.3

gAdjMatrix <- as.matrix(get.adjacency(A))
AN <- network(gAdjMatrix, directed=FALSE)
AT <- summary(AN ~ triangle)

gAdjMatrix <- as.matrix(get.adjacency(B))
BN <- network(gAdjMatrix, directed=FALSE)
BT <- summary(BN ~ triangle)

gAdjMatrix <- as.matrix(get.adjacency(C))
CN <- network(gAdjMatrix, directed=FALSE)
CT <- summary(CN ~ triangle)

gAdjMatrix <- as.matrix(get.adjacency(D))
DN <- network(gAdjMatrix, directed=FALSE)
DT <- summary(DN ~ triangle)

# Milton
fu <- function (u, theta1, theta2) theta1*u + theta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
uEstrela.A <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = theta1Aux, theta2 = theta2Aux[1] )
uEstrela.B <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = theta1Aux, theta2 = theta2Aux[6] )
uEstrela.C <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = theta1Aux, theta2 = theta2Aux[7] )
uEstrela.D <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = theta1Aux, theta2 = theta2Aux[9] )

#uEstrela.Tst <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = -0.8, theta2 = 1.1 )

par(mfrow=c(4,3))
plot(A, layout=layout.fruchterman.reingold.grid, vertex.color="green")
title(main = list(paste("A \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[1] ), cex = 1, font = 1))
plot(A, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("A \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[1] ), cex = 1, font = 1))
plot(A, layout=layout.drl , vertex.color="green")
title(main = list(paste("A \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[1] ), cex = 1, font = 1))

plot(B, layout=layout.fruchterman.reingold.grid, vertex.color="green")
title(main = list(paste("B \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[6] ), cex = 1, font = 1))
plot(B, layout=layout.reingold.tilford, vertex.color="green")#o importante
title(main = list(paste("B \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[6] ), cex = 1, font = 1))
plot(B, layout=layout.drl , vertex.color="green")
title(main = list(paste("B \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[6] ), cex = 1, font = 1))

plot(C, layout=layout.fruchterman.reingold.grid, vertex.color="green")
title(main = list(paste("C \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[7] ), cex = 1, font = 1))
plot(C, layout=layout.reingold.tilford, vertex.color="green")#o importante
title(main = list(paste("C \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[7] ), cex = 1, font = 1))
plot(C, layout=layout.drl , vertex.color="green")
title(main = list(paste("C \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[7] ), cex = 1, font = 1))

plot(D, layout=layout.fruchterman.reingold.grid, vertex.color="green")
title(main = list(paste("D \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[9] ), cex = 1, font = 1))
plot(D, layout=layout.reingold.tilford, vertex.color="green")#o importante
title(main = list(paste("D \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[9] ), cex = 1, font = 1))
plot(D, layout=layout.drl , vertex.color="green")
title(main = list(paste("D \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[9] ), cex = 1, font = 1))

# Adrian
par(mfrow=c(2,2))
plot(A, layout=layout.fruchterman.reingold.grid, vertex.color="green")
title(main = list(paste("A \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[1] ), cex = 1, font = 1))
plot(B, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("B \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[3] ), cex = 1, font = 1))
plot(C, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("C \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[6] ), cex = 1, font = 1))
plot(D, layout=layout.drl , vertex.color="green")
title(main = list(paste("D \nTheta1 : ", theta1Aux, "\nTheta2 : ", theta2Aux[9] ), cex = 1, font = 1))

# Milton
par(mfrow=c(2,2))
plot(A, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A), ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))
plot(B, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B), ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))
plot(C, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C), ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))
plot(D, layout=layout.reingold.tilford, vertex.color="green")
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D), ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))

# Milton
par(mfrow=c(2,2))
plot(degree.distribution(A, cumulative = FALSE), ylab = "distribution")
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A), ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))
plot(degree.distribution(B, cumulative = FALSE), ylab = "distribution")
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B), ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))
plot(degree.distribution(C, cumulative = FALSE), ylab = "distribution")
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C), ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))
plot(degree.distribution(D, cumulative = FALSE), ylab = "distribution")
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D), ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))

par(mfrow=c(2,2))
hist(degree(as.network.matrix(get.adjacency(A))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="")
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A),", Triang : ",AT, ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))
hist(degree(as.network.matrix(get.adjacency(B))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="")
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B),", Triang : ",BT, ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))
hist(degree(as.network.matrix(get.adjacency(C))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="")
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C),", Triang : ",CT, ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))
hist(degree(as.network.matrix(get.adjacency(D))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="")
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D),", Triang : ",DT , ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))

# Adrian
plot(degree.distribution(A, cumulative = FALSE), ylab = "distribution", main = "Grafo A")
plot(degree.distribution(D, cumulative = FALSE), ylab = "distribution", main = "Grafo D")
par(mfrow=c(2,2))
hist(degree(as.network.matrix(get.adjacency(A))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="Grafo A")
hist(degree(as.network.matrix(get.adjacency(B))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="Grafo B")
hist(degree(as.network.matrix(get.adjacency(C))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="Grafo C")
hist(degree(as.network.matrix(get.adjacency(D))), col="lightblue", xlim=c(0, 80),xlab="Vertex Degree", ylab="Frequency", main="Grafo D")

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

#############
# Comunidades
#############
kcA <- fastgreedy.community(A)
kcB <- fastgreedy.community(B)
kcC <- fastgreedy.community(C)
kcD <- fastgreedy.community(D)
kcA
kcB
kcC
kcD
sizes(kcA)
sizes(kcB)
sizes(kcC)
sizes(kcD)
par(mfrow=c(2,2))
plot(sizes(kcA))
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A), ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))
plot(sizes(kcB))
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B), ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))
plot(sizes(kcC))
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C), ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))
plot(sizes(kcD))
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D), ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))

par(mfrow=c(1,2))
plot(kcA,A,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A), ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))
V(A)$color <- kcA$membership+1
plot(A, edge.width=1,vertex.label=NA,vertex.size=6,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("A - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[1], "\nEdges : ", ecount(A), ", u :", round(uEstrela.A$maximum,3)), cex = 1, font = 1))

coords1 <- layout.kamada.kawai(A, dim=3)
coords2 <-layout.fruchterman.reingold(A, dim=3)
coords3 <-layout.reingold.tilford(A, dim=3)
rglplot(A,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords1)
rglplot(A,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords2)
rglplot(A,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords3)

tkplot(A,vertex.size=10, vertex.label.cex=1)

par(mfrow=c(1,2))
plot(kcB,B,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B), ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))
V(B)$color <- kcB$membership+1
plot(B, edge.width=1,vertex.label=NA,vertex.size=6,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("B - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[3], "\nEdges : ", ecount(B), ", Cresc.", round((ecount(B)-ecount(A))/ecount(B)*100,2), "%", ", u :", round(uEstrela.B$maximum,3)  ), cex = 1, font = 1))

coords1 <- layout.kamada.kawai(B, dim=3)
coords2 <-layout.fruchterman.reingold(B, dim=3)
coords3 <-layout.reingold.tilford(B, dim=3)
rglplot(B,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords1)
rglplot(B,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords2)
rglplot(B,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords3)

tkplot(B,vertex.size=10, vertex.label.cex=1)

par(mfrow=c(1,2))
plot(kcC,C,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C), ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))
V(C)$color <- kcC$membership+1
plot(C, edge.width=1,vertex.label=NA,vertex.size=6,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("C - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[7], "\nEdges : ", ecount(C), ", Cresc.", round((ecount(C)-ecount(B))/ecount(C)*100,2), "%", ", u :", round(uEstrela.C$maximum,3) ), cex = 1, font = 1))

coords1 <- layout.kamada.kawai(C, dim=3)
coords2 <-layout.fruchterman.reingold(C, dim=3)
coords3 <-layout.reingold.tilford(C, dim=3)
rglplot(c,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords1)
rglplot(c,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords2)
rglplot(c,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords3)

tkplot(C,vertex.size=10, vertex.label.cex=1)

par(mfrow=c(1,2))
plot(kcD,D,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D), ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))
V(D)$color <- kcD$membership+1
plot(D, edge.width=1,vertex.label=NA,vertex.size=6,asp=1,layout=layout.fruchterman.reingold)
title(main = list(paste("D - Theta1 : ", theta1Aux, ", Theta2 : ", theta2Aux[9], "\nEdges : ", ecount(D), ", Cresc.", round((ecount(D)-ecount(C))/ecount(D)*100,2), "%", ", u :", round(uEstrela.D$maximum,3)  ), cex = 1, font = 1))

coords1 <- layout.kamada.kawai(D, dim=3)
coords2 <-layout.fruchterman.reingold(D, dim=3)
coords3 <-layout.reingold.tilford(D, dim=3)
rglplot(D,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords1)
rglplot(D,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords2)
rglplot(D,edge.width=1,vertex.label=NA,vertex.size=8,asp=1,layout=coords3)

tkplot(D,vertex.size=10, vertex.label.cex=1)

# Adrian
plot(kcA,A,edge.width=2,vertex.label=NA)
plot(kcB,B,edge.width=2,vertex.label=NA)
plot(kcC,C,edge.width=2,vertex.label=NA)
plot(kcD,D,edge.width=2,vertex.label=NA)

par(mfrow=c(2,2))
dendPlot(kcA)
dendPlot(kcB)
dendPlot(kcC)
dendPlot(kcD)

