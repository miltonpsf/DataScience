# Gera grafo igraph
# Converte igraph para network
# Simula 

rm(list=ls())
library(igraph)
library(network)
library(Bergm)
library(mixer)
library(intergraph)


SimulaGrafos <- function (pNVertices, ptheta1, ptheta2, pgNetworkBase, pLimSupEigenValues) {
  # gIgraph <- erdos.renyi.game(nVertices, prob, type = "gnp")
  # gAdjMatrix <- as.matrix(get.adjacency(gIgraph))
  # gNetworkBase <- network(gAdjMatrix, directed=FALSE)
    
  gErgmSim <- simulate(gNetworkBase ~ edges + triangle, 
                      coef=c(ptheta1,ptheta2),nsim=100,
                      control=control.simulate(MCMC.burnin=1000, MCMC.interval=100),
                      statsonly=FALSE,
                      sequential = FALSE)
  
  nArestas <- attr(gErgmSim,"stats")[,1]
  nTriangulos <- attr(gErgmSim,"stats")[,2] 
  densidades <- as.matrix(lapply(gErgmSim, function(x) network.density(x)))
  return(list(nArestas, nTriangulos, densidades, gErgmSim))
}

nVertices <- 50
theta1    <- -0.8*2
prob      <- 0.5

gNetworkBase <- network(nVertices,density = prob, directed = FALSE)

theta2    <- 0.7*6/50
par(mfrow=c(1,1))

  grafosSimulados <- SimulaGrafos(pNVertices = nVertices, 
                                  ptheta1 = theta1, 
                                  ptheta2 = theta2, 
                                  gNetworkBase)
  
  nArestasSim <- grafosSimulados[[1]]
  nTriangulosSim <- grafosSimulados[[2]]
  densidadeSim <- grafosSimulados[[3]]
  
  plot(nArestasSim, nTriangulosSim, xlab = "edges", ylab = "triangles", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2, "\nProb : ", prob))
  plot(densidadeSim, main = paste("Densidade. Prob : ", prob))

  
theta2    <- c(0.5*6/50,0.6*6/50,0.7*6/50,0.8*6/50)
par(mfrow=c(4,2))
for (i in 1:length(theta2)) {
  grafosSimulados <- SimulaGrafos(pNVertices = nVertices, 
                                  ptheta1 = theta1, 
                                  ptheta2 = theta2[i], 
                                  gNetworkBase)
  
  nArestasSim <- grafosSimulados[[1]]
  nTriangulosSim <- grafosSimulados[[2]]
  densidadeSim <- grafosSimulados[[3]]
  
  plot(nArestasSim, nTriangulosSim, xlab = "edges", ylab = "triangles", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nProb : ", prob))
  plot(densidadeSim, main = paste("Densidade. Prob : ", prob))
  #boxplot(data=densidadeSim[,1])
}  


####################
# calcula Laplaciano
####################
gErgmSim <- grafosSimulados[[4]][50][[1]]
gIgraphSim <- asIgraph(gErgmSim)
gLaplac <- graph.laplacian(gIgraphSim)

#Centralidade
centralidade <- closeness(gIgraphSim,V(gIgraphSim))
hist(centralidade)
plot(centralidade)
#Densidade de triangulos
transitivity(gIgraphSim, type="undirected")
cv <- sd(centralidade)/mean(centralidade)

#####################
# Calcula autovalores
#####################
par(mfrow=c(1,1))
eigenLaplac <- eigen(gLaplac, only.values = TRUE)
plot(eigenLaplac$values)
plot(gErgmSim)


#########################################
# Identifica quantos grupos foram gerados 
#########################################
limSupEigenValues = 0.5
numGrupo <- length(eigenLaplac$values[which(eigenLaplac$values < limSupEigenValues,)])

####################################
# Gera uma mistura de grafos (mixer)
####################################
gAdjMatrixSim <- as.matrix(get.adjacency(gIgraphSim, attr=NULL, edges=FALSE, names=FALSE))
gMixer <- mixer(gAdjMatrixSim, qmin = numGrupo, qmax = numGrupo, method="bayesian")


