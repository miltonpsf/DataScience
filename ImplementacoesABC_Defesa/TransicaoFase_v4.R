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
theta1    <- -0.8*2
prob      <- 0.5

gNetworkBase <- network(nVertices,density = prob, directed = FALSE)

theta2    <- seq(0.5,1.5, by=0.1)*6/50
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
A<-asIgraph(grafosSimulados[[1]][[4]][1][[1]])
B<-asIgraph(grafosSimulados[[3]][[4]][1][[1]])
C<-asIgraph(grafosSimulados[[6]][[4]][1][[1]])
D<-asIgraph(grafosSimulados[[9]][[4]][1][[1]])
par(mfrow=c(2,2))
plot(A, layout=layout.fruchterman.reingold.grid, vertex.color="green")
plot(B, layout=layout.reingold.tilford, vertex.color="green")#o importante
plot(C, layout=layout.reingold.tilford, vertex.color="green")#o importante
plot(D, layout=layout.drl , vertex.color="green")
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
