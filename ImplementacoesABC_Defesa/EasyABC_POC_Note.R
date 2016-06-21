rm(list=ls())

#Sys.setlocale(category = "LC_ALL", locale = "pt_BR.UTF-8")
#library(gdata)
library(sand)
#library(ergm)
library(Bergm)
library(EasyABC)
library(lhs)
library(lmf)
#source("/home/ufmg/milton/MestradoEstatistica/9-Dissertacao/ABC/ImplementacoesABC_Defesa//EasyABCSource/EasyABC/R/EasyABC-internal.R")
#source("/home/ufmg/milton/MestradoEstatistica/9-Dissertacao/ABC/ImplementacoesABC_Defesa/EasyABC-internal.R")
#library(coda)
#library("entropy")
library("yaml")
#library("RJDBC")
#myDir<-"/home/ufmg/milton/MestradoEstatistica/9-Dissertacao/ABC/ImplementacoesABC_Defesa"
myDir<-"C:/Milton/Mestrado Estatistica/2014/9-Dissertacao/ABC/ImplementacoesABC_Defesa"
setwd(myDir)
#configFile <- paste(myDir, "config.yml", sep="/");
#config <- yaml.load_file(configFile)
#setwd(config$diretorios$workDir)
source("GrafosFunctions.R")

gWMsg <- ""
gEMsg <- ""

# Par?metros Gerais
listaNVertices <- c(50)
pNVertices      <- listaNVertices[[1]]
nVertices  <- 50
numGrafosGerar <- 1

thetas         <- c(0.2, 0.2)
#pProb           <- CalculaP(pTheta1)
tipoGerador     <- "Igraph" #Igraph , rbinom 
set.seed(10)
fu <- function (u, theta1, theta2) theta1*u + theta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
guEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = thetas[1], theta2 = thetas[2] )

pGrafoObs       <- GeraAmostraGrafo(pNVertices, guEstrela$maximum, tipo = tipoGerador) 

prob      <- 0.5
gNetworkBase <- network(pNVertices, density = prob, directed = FALSE)

tipoProcessamento <- "optimize" #simulate" #"optimize"
# ##################################
# Parametros ABC_sequential 
# ##################################
if (tipoProcessamento == "optimize") {
  pSeq_model  <- GraphModel_ABC_optimize
} else if (tipoProcessamento == "simulate") {
  pSeq_model  <- GraphModel_ABC_simulate
}
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
# Par?metros ABC_sequential Lenormand 
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
# ##################################
# Par?metros Bergm
# ##################################
pBergm_burn.in=500              # Default=100.number of burn-in iterations at the beginning of an MCMC run. If population MCMC is performed, it refers to the number of burn-in iterations for every chain of the population.
pBergm_main.iters=10850         # Count; number of iterations for the MCMC chain(s) excluding burn-in. If population
                                # MCMC is performed, it refers to the number of iterations for every chain
                                # of the population.
pBergm_aux.iters=10850          # Count; number of auxiliary iterations used for network simulation.
pBergm_m.prior=c(0)             # Mean of the multivariate Normal prior. By default set to a vector of 0's
pBergm_sigma.prior=diag(100,1)  # Variance/covariance matrix for the multivariate Normal prior. By default set to a diagonal matrix with every diagonal entry equal to 100.
pBergm_nchains=8                # Number of chains of the population MCMC. By default set to twice the
                                # model dimension (number of model terms). If the model is one-dimensional,
                                # nchains is set to 1.
pBergm_gamma=0.003               # Valor anterior=0.5.?parallel ADS move factor.? In case of one-dimensional models, the
                                # population MCMC procedure is disabled and gamma is used as variance of the
                                # Normal proposal distribution.
pBergm_sigma.epsilon=diag(0.0025,1) # Valor anterior=diag(0.1,1)variance/covariance matrix for the multivariate Normal proposal or ?parallel ADS move parameter?.
                                    # By default set to a diagonal matrix with every diagonal
                                    # entry equal to 0.0025. If the model is one-dimensional, sigma.espilon is
                                    # set equal to gamma.

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

if (tipoProcessamento == "optimize") {
  par(mfrow=c(1,2))
  ABC_sequencial_optimize <- ABC_sequencial
  hist(ABC_sequencial_optimize$param[,1], freq=FALSE,main = paste("Dist. a posterior de theta1 obs = 0.2. \nMédia a posteriori : ", round(mean(ABC_sequencial_optimize$param[,1]),2), "\nABC Seq Lenormand. 50 Vértices"), xlab="Theta 1")
  lines(density(ABC_sequencial_optimize$param[,1])$x,density(ABC_sequencial_optimize$param[,1])$y, col="red", lwd=2)
  hist(ABC_sequencial_optimize$param[,2], freq=FALSE,main = paste("Dist. a posterior de theta2 obs = 0.2. \nMédia a posteriori : ", round(mean(ABC_sequencial_optimize$param[,2]),2), "\nABC Seq Lenormand. 50 Vértices"), xlab="Theta 1")
  lines(density(ABC_sequencial_optimize$param[,2])$x,density(ABC_sequencial_optimize$param[,2])$y, col="green",lwd=2)
} else if (tipoProcessamento == "simulate") {
  ABC_sequencial_simulate <- ABC_sequencial
}
