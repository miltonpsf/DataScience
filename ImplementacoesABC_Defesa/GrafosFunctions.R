# Arquivo com as seguintes fun??es :
#   GeraEstSufGrafo   = Gera Estat?sticas Suficientes de uma lista de grafos
#   CalculaTheta1     = Calcula theta1 dado a probabilidade
#   CalculaP          = Calcula a probabilidade dado theta1   
#   GeraGrafo         = Gera grafo dado o n?mero de v?rtices e a probabilidade
#   GeraAmostraGrafo  = Gera Amostra de Grafos dado o numero de grafos a gerar
#   GraphModel_ABCSeq = Defini??o do modelo para o algoritmo ABC sequencial = c?lculo das estat?sticas suficientes de grafos simulados

GeraEstSufGrafo = function (pGrafo) {
  summary(pGrafo ~ edges + triangle)
}

CalculaTheta1 = function (pProb) {
  -log(1/pProb - 1)
}
CalculaP = function (pTheta1) {
  exp(pTheta1)/(1+exp(pTheta1))
}

GeraGrafo <- function (pNVertices, pProb,tipo) {
  if (tipo == "Igraph") {
    if (pProb < 0 || pProb > 1) {
      write.csv2(paste("Probabilidade : ",pNVertices,pProb),file=paste("Param_GeraGrafo",pProb,pNVertices))
    }  
    erdos.renyi.game(pNVertices, pProb, type = "gnp")
  }
  else if (tipo == "rbinom" ){ 
      tamAmostraBin <- pNVertices*pNVertices
      m <- matrix(rbinom(tamAmostraBin,1,pProb),pNVertices,pNVertices)
      diag(m) <- 0
      ind <- lower.tri(m)
      m[ind] <- t(m)[ind]
      graph.adjacency(m,mode="undirected",weighted = NULL)
      }
      else { 
        stop("Tipo de grafo inexistente.")
       }
}

GeraAmostraGrafo <- function (pNVertices, pProb, tipo) {
  gfAmostra <- GeraGrafo(pNVertices, pProb, tipo)
  return(gfAmostra)
}


GraphModel_ABC_optimize <- function (pThetas) {
  
  # Receber 2 par?metros : Theta1 e Theta2
  # Calcular u*
  thetas <- qlnorm(pThetas, meanlog=-log(4)/2, sdlog=sqrt(log(4)))
  #thetas <- qexp(pThetas, rate = 1.5)
  #thetas <- abs(qnorm(pThetas, mean=0, sd=sqrt(1)))
  #thetas <- 1 - sqrt(2/pi) + sqrt((3*pi)/(pi -2))*thetas
  fu <- function (u, beta1, beta2) beta1*u + beta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
  uEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, beta1 = thetas[1], beta2 = thetas[2] )
  # Passar u* para ao fun??o GeraAmostraGrafo
  # Alterar modelo Bergm para estimar edges e triangles
  #curve(dlnorm(x,meanlog=-1,sdlog=0.7),0,2)
  
  g <- GeraAmostraGrafo(pNVertices, uEstrela$maximum, tipo = tipoGerador)   
 
 if (is.igraph(g)) {
   gAdjMat <- as.matrix(get.adjacency(g))
   g <- as.network(gAdjMat, directed = FALSE)
   y <- GeraEstSufGrafo(g)
 } else {
    y <- Inf
   }
 y
}

GraphModel_ABC_simulate <- function (pThetas) {
  
  #thetas <- qlnorm(pThetas, meanlog=-1, sdlog=0.7)
  thetas <- qexp(pThetas, rate = 1.5)
  g <- simulate(gNetworkBase ~ edges + triangle, 
                       coef=c(thetas[1],thetas[2]),
                       control=control.simulate(MCMC.burnin=1000, MCMC.interval=100),
                       statsonly=FALSE,
                       sequential = FALSE)

  if (is.network(g)) {
    y <- GeraEstSufGrafo(g)
  } else {
    y <- Inf
  }
  y
}

