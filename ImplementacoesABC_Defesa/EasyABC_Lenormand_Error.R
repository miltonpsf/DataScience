rm(list=ls())

library(sand)
library(ergm)
library(network)
library(lhs)
library(lmf)
source("C:/Milton/Mestrado Estatistica/2014/9-Dissertacao/ABC/Implementacoes ABC_Defesa/EasyABC-internal.R")

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


GraphModel_ABC <- function (pThetas) {
  
  # Receber 2 parâmetros : Theta1 e Theta2
  # Calcular u*
  fu <- function (u, beta1, beta2) beta1*u + beta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
  uEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, beta1 = pThetas[1], beta2 = pThetas[2] )
  # Passar u* para ao função GeraAmostraGrafo
  # Alterar modelo Bergm para estimar edges e triangles

  g <- GeraAmostraGrafo(nVertices, uEstrela$maximum, tipo = tipoGerador)   
  nEhGrafo <- 0
  for (i in 1:length(g)) {
    if (is.igraph(g[[i]])) {
     nEhGrafo <- nEhGrafo + 1
   }
 }
 if (nEhGrafo == length(g)) {
   ehGrafo <- TRUE
 } else {
   ehGrafo <- FALSE
 }
 if (ehGrafo) {
   y <- GeraEstSufGrafo(g)
 } else {
    y <- Inf
   }
 y
}

listaNVertices <- c(50)
pNVertices      <- listaNVertices[[1]]
nVertices  <- 50
numGrafosGerar <- 1

thetas         <- c(0.2, 0.2)
tipoGerador     <- "Igraph" #Igraph , rbinom 
set.seed(10)
fu <- function (u, theta1, theta2) theta1*u + theta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
guEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = thetas[1], theta2 = thetas[2] )

pGrafoObs       <- GeraAmostraGrafo(pNVertices, guEstrela$maximum, tipo = tipoGerador) 

# ##################################
# ABC_sequential Params
# ##################################
pSeq_model           <- GraphModel_ABC
gAdjMatrix <- as.matrix(get.adjacency(pGrafoObs))
pGrafoObsNet <- as.network(gAdjMatrix, directed = FALSE)
pSummary_stat_target <- GeraEstSufGrafo(pGrafoObsNet)   # A vector containing the targeted (observed) summary statistics.
pPrior_test          <- " X1 > 0 & X2 > 0"  # A string expressing the constraints between model parameters.
pN_cluster           <- 1	         # a positive integer. If larger than 1 (the default value), ABC_sequential will
                                   # launch model simulations in parallel on n_cluster cores of the computer.
pUse_seed            <- FALSE      # If FALSE (default), ABC_sequential provides as input to the function
                                   # model a vector containing the model parameters used for the simulation. If TRUE,
                                   # ABC_sequential provides as input to the function model a vector containing an
                                   # integer seed value and the model parameters used for the simulation. In this last
                                   # case, the seed value should be used by model to initialize its pseudo-random
                                   # number generators (if model is stochastic).
pVerbose             <- FALSE	     # If TRUE, ABC_sequential writes in the current directory
                                   # intermediary results at the end of each step of the algorithm various files.
pdist_weights        <- NULL       # A vector containing the weights to apply to the distance between the computed
                                   # and the targeted statistics. These weights can be used to give more importance
                                   # to a summary statistisc for example. The weights will be normalized before
                                   # applying them. If not provided, no weights will be applied.
  
# ###################################
# ABC_sequential Lenormand Params
# ###################################
pLenormand_prior           <- list(c("unif",0,1),c("unif",0,1))
pLenormand_nb_simul          <- 1250 # A positive integer equal to the desired number of simulations of the model below the tolerance threshold when method is "Beaumont", "Drovandi" and "Delmoral".
                                     # When method is "Lenormand", the number of simulations below the tolerance threshold is equal to nb_simul * alpha
pLenormand_inside_prior      <- TRUE # If FALSE, parameter sampling is not restricted to the initial ranges of the prior distribution during the subsequent algorithm steps.
pLenormand_p_acc_min         <- 0.10 # This is the stopping criterion of the algorithm: a small number ensures a better
                                     # convergence of the algorithm, but at a cost in computing time. Default value is 0.05.  
                                     # A positive number between 0 and 1 (strictly) used when method is "Lenormand"
pLenormand_alpha             <- 0.8    # This is the proportion of particles kept at each step in the algorithms "Delmoral", "Lenormand"
                                     # Default values are 0.5 when method is "Drovandi", "Lenormand"

ABC_sequential <- function(method, model, prior, nb_simul, summary_stat_target, prior_test = NULL, 
    n_cluster = 1, use_seed = FALSE, verbose = FALSE, dist_weights=NULL, ...) {
    ## checking errors in the inputs
    if (missing(method)) 
        stop("'method' is missing")
    if (missing(model)) 
        stop("'model' is missing")
    if (missing(prior)) 
        stop("'prior' is missing")
    data = .wrap_constants_in_model(prior, model, use_seed)
    prior = data$new_prior
    model = data$new_model
    prior = .process_prior(prior)
    if (!is.null(prior_test)) 
        .check_prior_test(length(prior), prior_test)
    if (missing(nb_simul)) 
        stop("'nb_simul' is missing")
    if (missing(summary_stat_target)) 
        stop("'summary_stat_target' is missing")
    if (!any(method == c("Beaumont", "Drovandi", "Delmoral", "Lenormand", "Emulation"))) {
        stop("Method must be Beaumont, Drovandi, Delmoral, Lenormand or Emulation")
    }
    if (!is.vector(nb_simul)) 
        stop("'nb_simul' has to be a number.")
    if (length(nb_simul) > 1) 
        stop("'nb_simul' has to be a number.")
    if (nb_simul < 1) 
        stop("'nb_simul' must be a number larger than 1.")
    nb_simul = floor(nb_simul)
    if (!is.vector(summary_stat_target)) 
        stop("'summary_stat_target' has to be a vector.")
    if (!is.vector(n_cluster)) 
        stop("'n_cluster' has to be a number.")
    if (length(n_cluster) > 1) 
        stop("'n_cluster' has to be a number.")
    if (n_cluster < 1) 
        stop("'n_cluster' has to be a positive number.")
    n_cluster = floor(n_cluster)
    if (!is.logical(use_seed)) 
        stop("'use_seed' has to be boolean")
    if (!is.logical(verbose)) 
        stop("'verbose' has to be boolean")
    if (!is.null(dist_weights) && length(dist_weights)!=length(summary_stat_target)) {
        stop("'dist_weights' has to be the same length than 'summary_stat_target'")
    }
    sequential = NULL
    if (n_cluster == 1) {
        sequential = .ABC_sequential(method, model, prior, prior_test, nb_simul, 
            summary_stat_target, use_seed, verbose, dist_weights=dist_weights, ...)
    } else {
        if (method=="Emulation") {
            stop("'Emulation' method isn't yet available in 'cluster' mode'")
        }
        if (use_seed == FALSE) {
            stop("For parallel implementations, you must specify the option 'use_seed=TRUE' and modify your model accordingly - see the package's vignette for more details.")
        }
        sequential = .ABC_sequential_cluster(method, model, prior, prior_test, nb_simul, 
            summary_stat_target, n_cluster, use_seed, verbose, dist_weights=dist_weights, ...)
    }
    sequential
} 


ABC_sequencial <-ABC_sequential(method              = "Lenormand", 
                                model               = pSeq_model, 
                                prior               = pLenormand_prior,
                                nb_simul            = pLenormand_nb_simul, 
                                summary_stat_target = pSummary_stat_target, 
                                prior_test          = pPrior_test,
                                n_cluster           = pN_cluster ,
                                use_seed            = pUse_seed  ,
                                verbose             = pVerbose   ,
                                dist_weights        = pdist_weights,
                                alpha               = pLenormand_alpha, 
                                inside_prior        = pLenormand_inside_prior, 
                                p_acc_min           = pLenormand_p_acc_min)
