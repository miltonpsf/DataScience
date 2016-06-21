# Simula grafo informando theta1 e theta2
# calcula Laplaciano
# Calcula autovalores
# Identifica quantos grupos foram gerados 
# Gera uma mistura de grafos (mixer)
# Histograma dos Taus da mistura
# Gera grafo affiliation com os alphas da mistura. lambda = ? , epsilon = ?

library(igraph)
library(network)
library(Bergm)
library(mixer)
library(intergraph)
#ergm to mixer: theta1<0 e theta2>0 to alpha taus
rm(list=ls())

GeraMistura <- function (pN, ptheta1, ptheta2, pDensidade, pLimSupEigenValues) {
  pN         = 50
  ptheta1    = -4
  ptheta2    = 1
  pDensidade = 0.3
  pLimSupEigenValues = 0.5
  n=pN
  theta1<- ptheta1
  theta2<- ptheta2
  gBase <- network(n,density = pDensidade, directed = FALSE)
  network.size(gBase)       
  network.edgecount(gBase)  
  network.density(gBase)    
  network.dyadcount(gBase)  
  
  #########################################
  # Simula grafo informando theta1 e theta2
  #########################################
  gSim <- simulate( ~ edges + triangle, 
                    coef=c(theta1,theta2),nsim=1,
                    control=control.simulate(MCMC.burnin=1000, MCMC.interval=100), basis=gBase)
  # gSim
  # plot(gSim)
  gSimIg <- asIgraph(gSim)
  
  ####################
  # calcula Laplaciano
  ####################
  gLaplac <- graph.laplacian(gSimIg)
  
  #####################
  # Calcula autovalores
  #####################
  eigenLaplac <- eigen(gLaplac, only.values = TRUE)
  
  #########################################
  # Identifica quantos grupos foram gerados 
  #########################################
  Numgrupo <- length(eigenLaplac$values[which(eigenLaplac$values < pLimSupEigenValues,)])
  gAdj <- as.matrix(get.adjacency(gSimIg, attr=NULL, edges=FALSE, names=FALSE))
  
  ####################################
  # Gera uma mistura de grafos (mixer)
  ####################################
  if (Numgrupo > n) {
    Numgrupo <- n
  }
  gMixer <- mixer(gAdj, qmin = Numgrupo, qmax = Numgrupo, method="bayesian")
  return(gMixer)
}

N <- 50
theta1 <- seq(-4,-0.5, by = 0.5)
theta2 <- seq(0.5, 4, by = 0.5) 
densidade <- 0.3
limSupEigenValues <- 0.5
gMisturaAlphas <- array(list(),c(length(theta1),length(theta2)))
gMisturaTaus <- array(list(),c(length(theta1),length(theta2)))
Numgrupo <- matrix(nrow = length(theta1), ncol = length(theta2))
for (i in 1:length(theta1)) {
  for ( j in 1:length(theta2)) {
    aux <- GeraMistura(N, theta1[i], theta2[j], densidade, limSupEigenValues)
    gMisturaAlphas[[i,j]] <- aux$output[[1]]$alphas
    gMisturaTaus[[i,j]] <- aux$output[[1]]$Taus
    Numgrupo[i,j] <- nrow(aux$output[[1]]$Taus)
  }
}

for (i in 1:length(theta1)) {
  for ( j in 1:length(theta2)) {
    par(mfrow=c(1,1))
    hist(gMisturaTaus[[i,j]],main = paste("Taus [i : j] = ",i,":",j),xlab = "Taus")
    gAff <- graph.affiliation( N, alphaVect= c(gMisturaAlphas[[i,j]], 0.3), 
                               lambda=0.7,
                               epsilon=0.05, 
                               directed=FALSE )
    G <- network(gAff$x)
    plot(G, main = paste("Grafo Taus [i:j] = ",i,":",j),xlab = "Taus")
    plot(gAff$x, main = paste("Affiliation Taus [i:j] = ",i,":",j),xlab = "Taus")
    plot(gAff$cluster, main = paste("Cluster Taus [i:j] = ",i,":",j),xlab = "Taus")
  }
}

