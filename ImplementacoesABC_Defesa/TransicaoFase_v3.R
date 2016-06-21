# Gera grafo igraph
# Converte igraph para network
# Simula 

rm(list=ls())
library(igraph)
library(network)
library(Bergm)
library(mixer)
library(intergraph)
nGrafosSim <- 100

SimulaGrafos <- function (pNVertices, ptheta1, ptheta2, pgNetworkBase, pLimSupEigenValues) {
  # gIgraph <- erdos.renyi.game(nVertices, prob, type = "gnp")
  # gAdjMatrix <- as.matrix(get.adjacency(gIgraph))
  # gNetworkBase <- network(gAdjMatrix, directed=FALSE)
    
  gErgmSim <- simulate(gNetworkBase ~ edges + triangle, 
                      coef=c(ptheta1,ptheta2),nsim=nGrafosSim,
                      control=control.simulate(MCMC.burnin=1000, MCMC.interval=100),
                      statsonly=FALSE,
                      sequential = FALSE)
  
  nArestas <- attr(gErgmSim,"stats")[,1]
  nTriangulos <- attr(gErgmSim,"stats")[,2] 
  densidades <- lapply(gErgmSim, function(x) network.density(x))
  return(list(nArestas, nTriangulos, densidades, gErgmSim))
}

nVertices <- 50
theta1    <- -0.8*2
prob      <- 0.5

gNetworkBase <- network(nVertices,density = prob, directed = FALSE)

theta2    <- c(0.5*6/50,0.6*6/50,0.7*6/50,0.8*6/50)
mediaArestas     <- list()
mediaTriangulos  <- list()
mediaDensidade   <- list()
sdArestas        <- list()
sdTriangulos     <- list()
sdDensidade      <- list()
nArestasSim      <- list()
nTriangulosSim   <- list()
densidadeSim     <- list()
grafosSimulados  <- list()  
for (i in 1:length(theta2)) {
  grafosSimulados[[i]]       <- SimulaGrafos(pNVertices = nVertices, 
                                  ptheta1 = theta1, 
                                  ptheta2 = theta2[i], 
                                  gNetworkBase)
  
  nArestasSim[[i]]      <- grafosSimulados[[i]][[1]]
  nTriangulosSim[[i]]   <- grafosSimulados[[i]][[2]]
  densidadeSimM       <- matrix(nrow=100,ncol=1)
  for (j in 1:100) {
    densidadeSimM[j,] <- as.numeric(grafosSimulados[[i]][[3]][j])
  }
  densidadeSim[[i]]     <- densidadeSimM

  mediaArestas[[i]]     <- mean(nArestasSim[[i]])
  mediaTriangulos[[i]]  <- mean(nTriangulosSim[[i]])
  mediaDensidade[[i]]   <- mean(densidadeSimM)
  sdArestas[[i]]        <- sd(nArestasSim[[i]])
  sdTriangulos[[i]]     <- sd(nTriangulosSim[[i]])
  sdDensidade[[i]]      <- sd(densidadeSimM) 
}  

par(mfrow=c(2,3))
for (i in 1:length(theta2)) {
  plot(as.matrix(nArestasSim[[i]]), as.matrix(nTriangulosSim[[i]]), xlab = "edges", ylab = "triangles", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nProb : ", prob))
  plot(densidadeSim[[i]], main = paste("Densidade. Prob : ", prob))
  auxBoxPlot <- as.data.frame(densidadeSimM[,1])
  boxplot(auxBoxPlot)
}

####################
# calcula Laplaciano
####################
centralidade   <- list()
centralidadeG   <- list()
transitividade <- list()
transitividadeG <- list()
cv             <- list()
cvG           <- list()
gIgraphSim     <- list()
gLaplac        <- list()
eigenLaplac    <- list()
eigenLaplacG   <- list()
for (i in 1:length(theta2) ) {
  for(j in 1:nGrafosSim) {
    gErgmSim <- grafosSimulados[[i]][[4]][j][[1]]
    gIgraphSim[[j]] <- asIgraph(gErgmSim)
    gLaplac[[j]] <- graph.laplacian(gIgraphSim[[j]])
    eigenLaplac[[j]] <- eigen(gLaplac[[j]], only.values = TRUE)
    
    #Centralidade
    centralidade[[j]] <- closeness(gIgraphSim[[j]],V(gIgraphSim[[j]]))
    #Densidade de triangulos
    transitividade[[j]] <- transitivity(gIgraphSim[[j]], type="undirected")
    cv[[j]] <- sd(centralidade[[j]])/mean(centralidade[[j]])
  }
  transitividadeG[[i]] <- transitividade
  centralidadeG[[i]] <- centralidade
  cvG[[i]] <- cv
  eigenLaplacG[[i]] <- eigenLaplac
}

par(mfrow=c(2,3))
for (i in 1:length(theta2)) {
  plot(as.matrix(transitividadeG[[i]]), xlab = "vértices", ylab = "transtividade", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nTransitividade"))
  for (j in 1:nGrafosSim) {
    plot(as.matrix(centralidadeG[[i]][[j]]), xlab = "vértices", ylab = "Centralidade", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nCentralidade Grafo : ", j))
  }
}

for (i in 1:length(theta2)) {
  plot(as.matrix(cvG[[i]]), ylab = "Coeficiente", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nCoeficiente de Variação"))
}

#####################
# Calcula autovalores
#####################
par(mfrow=c(2,3))
for (i in 1:length(theta2)) {
  for (j in 1:nGrafosSim) {
   plot(eigenLaplacG[[i]][[j]]$values, xlab = "vértices", ylab = "Autovalores", main = paste("Theta1 : ", theta1, " - Theta2 : ",theta2[i], "\nAutovalores Grafo : ", j))
  }  
}


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


