rm(list=ls())

#Sys.setlocale(category = "LC_ALL", locale = "pt_BR.UTF-8")
library(gdata)
library(sand)
library(ergm)
library(Bergm)
library(EasyABC)
#source("EasyABC-internal.R")
library(coda)
library("entropy")
library("yaml")
library("RJDBC")
myDir<-"/home/ufmg/milton/MestradoEstatistica/9-Dissertacao/ABC/ImplementacoesABC_Defesa"
#myDir<-"C:/Milton/Mestrado Estatistica/2014/9-Dissertacao/ABC/ImplementacoesABC_Defesa"
configFile <- paste(myDir, "config.yml", sep="/");
config <- yaml.load_file(configFile)
setwd(config$diretorios$workDir)
source("GrafosFunctions.R")
source("GeraAmostra_APosteriori_ABCSeq_Grafos.R")
source("FuncoesAcessaDadosBD.R")
source("~/MestradoEstatistica/9-Dissertacao/ABC/ImplementacoesABC_Defesa/Bergm/R/bergm.R")
# library(animation)
# library(ks)

gWMsg <- ""
gEMsg <- ""

# Par?metros Gerais
listaNVertices <- c(50)
pNVertices      <- listaNVertices[[1]]
numGrafosGerar <- 1

thetas         <- c(0.2, 0.2)
#pProb           <- CalculaP(pTheta1)
tipoGerador     <- "Igraph" #Igraph , rbinom 
set.seed(10)
fu <- function (u, theta1, theta2) theta1*u + theta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)
guEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = thetas[1], theta2 = thetas[2] )

if (pNVertices == 50) {
  load("pGrafoObs50.RData")
} else if (pNVertices == 100) {
  load("pGrafoObs100.RData")
} else if (pNVertices == 200) {
  load("pGrafoObs200.RData")
} else if (pNVertices == 500) {
  load("pGrafoObs500.RData")
} else if (pNVertices == 750) {
  load("pGrafoObs750.RData")
} else if (pNVertices == 1000) {
  load("pGrafoObs1000.RData")
}
# if (vcount(pGrafoObs) != pNVertices) {
#   pGrafoObs       <- GeraAmostraGrafo(pNVertices, guEstrela$maximum, tipo = tipoGerador) 
#   save(pGrafoObs, file = "pGrafoObs.RData")
# }

algoritmosModelos <- list(#c("ABC_sequencial_2thetas","Lenormand")
                          c("Bergm_2thetas","Bergm")
                          )
tipoProcessamento <- "Bergm" #Bergm" "simulate" "optimize"
if (tipoProcessamento == "simulate") {
  load("gNetworkBase.RData")
  if (vcount(pGrafoObs) != pNVertices) {
    prob      <- guEstrela$maximum
    gNetworkBase <- network(pNVertices, density = prob, directed = FALSE)
    save(gNetworkBase, file = "gNetworkBase.RData")
  }
}
# ##################################
# Par?metros ABC_sequential 
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
pBergm_familia = "LogNormal"
pBergm_burn.in=10000              # Default=100.number of burn-in iterations at the beginning of an MCMC run. If population MCMC is performed, it refers to the number of burn-in iterations for every chain of the population.
pBergm_main.iters=200000         # Count; number of iterations for the MCMC chain(s) excluding burn-in. If population
                                # MCMC is performed, it refers to the number of iterations for every chain
                                # of the population.
pBergm_aux.iters=2000          # Count; number of auxiliary iterations used for network simulation.
pBergm_m.prior=c(1,1)             # Mean of the multivariate Normal prior. By default set to a vector of 0's
pBergm_sigma.prior=diag(3,2)  # Variance/covariance matrix for the multivariate Normal prior. By default set to a diagonal matrix with every diagonal entry equal to 100.
pBergm_nchains=16               # Number of chains of the population MCMC. By default set to twice the
                                # model dimension (number of model terms). If the model is one-dimensional,
                                # nchains is set to 1.
pBergm_gamma=0.003               # Valor anterior=0.5.?parallel ADS move factor.? In case of one-dimensional models, the
                                # population MCMC procedure is disabled and gamma is used as variance of the
                                # Normal proposal distribution.
pBergm_sigma.epsilon=diag(0.0025,2) # Valor anterior=diag(0.1,1)variance/covariance matrix for the multivariate Normal proposal or ?parallel ADS move parameter?.
                                    # By default set to a diagonal matrix with every diagonal
                                    # entry equal to 0.0025. If the model is one-dimensional, sigma.espilon is
                                    # set equal to gamma.

# ####
amostra <- array(list(NULL),length(algoritmosModelos))
medidaKL1 <- array(0,length(algoritmosModelos))
KSTeste1  <- list(length(algoritmosModelos))
CHTeste1  <- array(0,length(algoritmosModelos))
tamAmostras1 <- array(0,length(algoritmosModelos))

medidaKL2 <- array(0,length(algoritmosModelos))
KSTeste2  <- list(length(algoritmosModelos))
CHTeste2  <- array(0,length(algoritmosModelos))
tamAmostras2 <- array(0,length(algoritmosModelos))

set.seed(10)
alphaTeorica  <- pSummary_stat_target[1] + 1
betaTeorica   <- choose(listaNVertices[[1]],2) - alphaTeorica + 1
mediaTeorica  <- alphaTeorica/(alphaTeorica+betaTeorica)
dataArquivo <- Sys.time()
plotResultadosCoda <- function (pAmostra, pNVertices, pmaxXParam1, pminXParam1, pmaxLag, pmedidaKL1,  pKSTeste1, pCHTeste1,
                                                      pmaxXParam2, pminXParam2, pmedidaKL2,  pKSTeste2, pCHTeste2) {
  par(mfrow=c(length(algoritmosModelos),3))
  for (j in 1:length(pAmostra)) {
    algoritmoMetodoAux <- pAmostra[[j]][1]
    algoritmoResultado <- pAmostra[[j]][2]
    if (!is.null(algoritmoMetodoAux)) {
      if (   algoritmoMetodoAux[[1]][1] == "ABC_sequencial_2thetas" ) {
        amostraTheta1 <- algoritmoResultado[[1]]$param[,1]
        amostraTheta2 <- algoritmoResultado[[1]]$param[,2]
        tempoProcessamento <- round(algoritmoResultado[[1]]$computime,2)
        epsilon <- algoritmoResultado[[1]]$epsilon
      } else if (algoritmoMetodoAux[[1]][1] == "Bergm_2thetas"){
        amostraTheta1 <- apply(algoritmoResultado[[1]]$Theta[,1,],MARGIN = 1, mean)
        amostraTheta2 <- apply(algoritmoResultado[[1]]$Theta[,2,],MARGIN = 1, mean)
        tempoProcessamento <- round(algoritmoResultado[[1]]$computime,2)
        epsilon <- 999
      }
       denAmostraTheta1 <- density(amostraTheta1)
       denAmostraTheta2 <- density(amostraTheta2)
#       xbeta1 <- seq(pminXParam1,pmaxXParam1,by=0.0001)
#       ybeta1 <- dbeta(xbeta1,alphaTeorica,betaTeorica)
#       maxYParamAmostra1 <- max(denAmostraTheta1$y,ybeta1)
#       minYParamAmostra1 <- min(denAmostraTheta1$y,ybeta1)
#       if (maxYParam1 < maxYParamAmostra1) {
#         maxYParam1 <- maxYParamAmostra1
#       }
#       if (minYParam1 > minYParamAmostra1) {
#         minYParam1 <- minYParamAmostra1
#       }
      maxYParam1 <- max(denAmostraTheta1$y)
      minYParam1 <- min(denAmostraTheta1$y)

#       xbeta2 <- seq(pminXParam2,pmaxXParam2,by=0.0001)
#       ybeta2 <- dbeta(xbeta2,alphaTeorica,betaTeorica)
#       maxYParamAmostra2 <- max(denAmostraTheta2$y,ybeta2)
#       minYParamAmostra2 <- min(denAmostraTheta2$y,ybeta2)
#       if (maxYParam2 < maxYParamAmostra2) {
#         maxYParam2 <- maxYParamAmostra2
#       }
#       if (minYParam2 > minYParamAmostra2) {
#         minYParam2 <- minYParamAmostra2
#       }      
      maxYParam2 <- max(denAmostraTheta2$y)
      minYParam2 <- min(denAmostraTheta2$y)

      media1 <- mean(amostraTheta1)
      dp1 <- sd(amostraTheta1)
      cv1 <- dp1/media1
      amostraMCMC1 <- mcmc(amostraTheta1)
      neff1 <- effectiveSize(amostraMCMC1)

      media2 <- mean(amostraTheta2)
      dp2 <- sd(amostraTheta2)
      cv2 <- dp2/media2
      amostraMCMC2 <- mcmc(amostraTheta2)
      neff2 <- effectiveSize(amostraMCMC2)
      
      tMediaTeorica <- iconv("\n Méd.Teórica : ",from = "UTF-8", to = "")
      tMediaTeorica <- "\n Méd.Teórica : "
      tMetodo <- iconv("Método : ",from = "UTF-8", to = "")
      tMetodo <- "Método : "
      tMediaPosteriori <- iconv(" Méd.a posteriori = ",from = "UTF-8", to = "")
      tMediaPosteriori <- " Méd.a posteriori = "
      tNumVertices <- iconv(" N.Nós = ",from = "UTF-8", to = "")
      tNumVertices <- " N.Nós = "
      tAutocorrelacao <- iconv("Autocorrelação. \n Taxa de Aceitação : ",from = "UTF-8", to = "")
      tAutocorrelacao <- paste("Autocorrelação. \n Taxa de Aceitação : ")
      par(mfrow = c(2,3))
      densplot(amostraMCMC1,type="l", lwd = 2,
                                      xlab = paste("Theta1 - ",tipoProcessamento),
                                      ylab = "T=Azul e A=Preta",
                                      xlim = c(pminXParam1,pmaxXParam1),
                                      ylim = c(minYParam1,maxYParam1),
                                      main = paste(tMetodo, 
                                      #algoritmoMetodoAux[[1]][1], " - ", 
                                      algoritmoMetodoAux[[1]][2], " - ", tipoGerador,
                                      "Theta1 = ",thetas[1],
                                      #" , Prob = ",round(CalculaP(pTheta1),3),
                                      "\n ",tMediaPosteriori, round(as.numeric(amostra[[j]][3]),3),
                                      #"\n HDR = 50%, 95% e 99%",
                                      #tMediaTeorica,round(mediaTeorica,3),
                                      tNumVertices, pNVertices,
                                      ", Tmp = ", tempoProcessamento,"s"))
      #lines(xbeta1,ybeta1,lwd=2,col="red")
      acceptanceRate1 <- 1 - rejectionRate(amostraMCMC1)
      #traceplot(amostraMCMC1, ylim=c(pminXParam1,pmaxXParam1), main = paste("KL-Test  : ", round(pmedidaKL1[j],6),"\n KS-Test : ", round(pKSTeste1[[j]]$statistic,4),", p-value :",round(pKSTeste1[[j]]$p.value,12),"\n  Chi-Test : ", round(pCHTeste1[[j]],4), ", CV : ",round(cv1,4), sep=""))
      traceplot(amostraMCMC1, ylim=c(pminXParam1,pmaxXParam1), main = "Convergência")
      autocorr.plot(amostraMCMC1, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste(tAutocorrelacao, round(acceptanceRate1,4),"\n neff : ",round(neff1,2),sep=""))

      densplot(amostraMCMC2,type="l", lwd = 2,
               xlab = paste("Theta2 - ",tipoProcessamento),
               ylab = "T=Azul e A=Preta",
               xlim = c(pminXParam2,pmaxXParam2),
               ylim = c(minYParam2,maxYParam2),
               main = paste(tMetodo, 
                            #algoritmoMetodoAux[[1]][1], " - ", 
                            algoritmoMetodoAux[[1]][2], " - ", tipoGerador,
                            "Theta2 = ",thetas[2],
                            #" , Prob = ",round(CalculaP(pTheta1),3),
                            "\n ",tMediaPosteriori, round(as.numeric(amostra[[j]][4]),3),
                            #"\n HDR = 50%, 95% e 99%",
                            #tMediaTeorica,round(mediaTeorica,3),
                            tNumVertices, pNVertices,
                            ", Tmp = ", tempoProcessamento,"s"))
      
      #lines(xbeta2,ybeta2,lwd=2,col="red")
      acceptanceRate2 <- 1 - rejectionRate(amostraMCMC2)
      #traceplot(amostraMCMC2, ylim=c(pminXParam2,pmaxXParam2), main = paste("KL-Test  : ", round(pmedidaKL2[j],6),"\n KS-Test : ", round(pKSTeste2[[j]]$statistic,4),", p-value :",round(pKSTeste2[[j]]$p.value,12),"\n  Chi-Test : ", round(pCHTeste2[[j]],4), ", CV : ",round(cv2,4), sep=""))
      traceplot(amostraMCMC2, ylim=c(pminXParam2,pmaxXParam2), main = "Convergência")
      autocorr.plot(amostraMCMC2, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste(tAutocorrelacao, round(acceptanceRate2,4),"\n neff : ",round(neff2,2),sep=""))

      # Mixing cadeia

    
      if (   algoritmoMetodoAux[[1]][1] == "ABC_sequencial_2thetas" ) {
        save(algoritmoResultado, file = paste("algoritmoResultado_Lenorman",pNVertices,"_",dataArquivo,".RData",sep=""))
        save(amostraTheta1, file = paste("amostraTheta1_Lenorman",pNVertices,"_",dataArquivo,".RData",sep=""))
        save(amostraTheta2, file = paste("amostraTheta2_Lenorman",pNVertices,"_",dataArquivo,".RData",sep=""))
      } else if (algoritmoMetodoAux[[1]][1] == "Bergm_2thetas"){
        save(algoritmoResultado, file = paste("algoritmoResultado_Bergm",pNVertices,"_",dataArquivo,".RData",sep=""))
        save(amostraTheta1, file = paste("amostraTheta1_Bergm",pNVertices,"_",dataArquivo,".RData",sep=""))
        save(amostraTheta2, file = paste("amostraTheta2_Bergm",pNVertices,"_",dataArquivo,".RData",sep=""))
      }
# 
#       noFile <- RetNomeArq(algoritmosModelos,pNVertices,tipoGerador, pTipoImpressao = "Coda")
#       ind <- seq(from=1,to=NROW(amostraTheta1), by=50)
#       x <- amostraTheta1[ind]
#       y <- amostraTheta2[ind]
#       fit <- lm(y ~ x )
# #       xs <- sort(x)
# #       ys <- sort(y)
# #       f <- function(x, y) {
# #         r <- kde(cbind(x,y), compute.cont=TRUE, eval.points = cbind(x,y))
# #         r <- r[[3]]
# #       }
# #       z <- outer(xs,ys,f)
# #           contour(xs,
# #                   ys,
# #                   z,
# #                   add=FALSE)
#       xMin <- min(x)
#       xMax <- max(x)
#       yMin <- min(y)
#       yMax <- max(y)
#       par(mfrow=c(1,1))
#       
#       printLine <- function (N=1000) {
#         for(i in 3:N) {
#           plot(x[-(i:N)],y[-(i:N)], xlim = c(xMin,xMax),ylim = c(yMin,yMax), col="red", pch=20)
# #           contour(xs,
# #                   ys,
# #                   z,
# #                   add=TRUE)
#           lines(x[-(i:N)],y[-(i:N)])
#           #Sys.sleep(1)
#         }
#       }
#       N <- NROW(x)
#       saveGIF(printLine(N) , movie.name = paste(noFile,".gif",sep=""), interval = 0.1, nmax = 30, ani.width = 700, ani.height = 600)


#       DT_PROCESSAMENTO          <- substr(dataArquivo,1,19)
#       NO_ALGORITMO    					<- algoritmoMetodoAux[[1]][1]
#       NO_MODELO     						<- algoritmoMetodoAux[[1]][2]
#       VL_THETA1     						<- thetas[1]
#       VL_PROBABILIDADE       		<- 0 #pProb
#       VL_MEDIA_POSTERIORI       <- as.numeric(amostra[[j]][3])
#       VL_MEDIA_TEORICA          <- mediaTeorica          
#       NU_VERTICES               <- pNVertices
#       NU_SEG_PROCESSAMENTO      <- as.numeric(tempoProcessamento)
#       TX_ACEITACAO              <- as.numeric(acceptanceRate1)
#       KL_TEST_EST               <- as.numeric(pmedidaKL1[j])
#       KS_TEST_EST               <- as.numeric(pKSTeste1[[j]]$statistic)
#       KS_TEST_PVALUE            <- as.numeric(pKSTeste1[[j]]$p.value)
#       CHISQ_TEST_EST            <- as.numeric(pCHTeste1[[j]])
#       NEFF                      <- as.numeric(neff1)
#       CV                        <- as.numeric(cv1)
#       ALPHA_TEORICA             <- as.numeric(alphaTeorica)
#       BETA_TEORICA              <- as.numeric(betaTeorica)
#       TIPO_PROC                 <- tipoProcessamento
#       VL_THETA2       					<- thetas[2]
#       conexao  <- RetornaConexaoBDOracle( config )
#       
#       comandoInsert <- paste ('INSERT INTO DBDMINFOSAS.TB_DABC_RESULTADO (DT_PROCESSAMENTO,NO_ALGORITMO, NO_MODELO, VL_THETA1, VL_PROBABILIDADE, VL_MEDIA_POSTERIORI , 
#                                  VL_MEDIA_TEORICA,NU_VERTICES, NU_SEG_PROCESSAMENTO, TX_ACEITACAO , KL_TEST_EST, KS_TEST_EST, KS_TEST_PVALUE, CHISQ_TEST_EST,NEFF,CV,ALPHA_TEORICA,BETA_TEORICA,TIPO_PROC,VL_THETA2  ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)')   
#       resultBD <- dbSendUpdate (conexao, comandoInsert,DT_PROCESSAMENTO,
#                                 NO_ALGORITMO  						,
#                                 NO_MODELO     						,
#                                 VL_THETA1     						,
#                                 VL_PROBABILIDADE       		,           
#                                 VL_MEDIA_POSTERIORI       ,           
#                                 VL_MEDIA_TEORICA          ,            
#                                 NU_VERTICES               ,
#                                 NU_SEG_PROCESSAMENTO      ,
#                                 TX_ACEITACAO              ,
#                                 KL_TEST_EST               ,
#                                 KS_TEST_EST               ,
#                                 KS_TEST_PVALUE            ,
#                                 CHISQ_TEST_EST,
#                                 NEFF,
#                                 CV,
#                                 ALPHA_TEORICA,
#                                 BETA_TEORICA,
#                                 TIPO_PROC,VL_THETA2)
#       
#       dbCommit(conexao)
#       am <- as.data.frame(amostraTheta1)
#       am$NO_ALGORITMO <- algoritmoMetodoAux[[1]][1]
#       am$NO_MODELO    <- algoritmoMetodoAux[[1]][2]
#       am$DT_PROCESSAMENTO <- DT_PROCESSAMENTO
#       am$THETA <- 1
#       names(am) <- c("VL_AMOSTRA_THETA", "NO_ALGORITMO" , "NO_MODELO" ,  "DT_PROCESSAMENTO", "THETA")
#       
#       tabelaBD <- "TB_DABC_AMOSTRA_POSTERIORI" 
#       dbWriteTable(conexao, tabelaBD,am, append=TRUE,row.names=FALSE, overwrite=FALSE,header=TRUE)
#       
#       am <- as.data.frame(amostraTheta2)
#       am$NO_ALGORITMO <- algoritmoMetodoAux[[1]][1]
#       am$NO_MODELO    <- algoritmoMetodoAux[[1]][2]
#       am$DT_PROCESSAMENTO <- DT_PROCESSAMENTO
#       am$THETA <- 2
#       names(am) <- c("VL_AMOSTRA_THETA", "NO_ALGORITMO" , "NO_MODELO" ,  "DT_PROCESSAMENTO", "THETA")
#       
#       tabelaBD <- "TB_DABC_AMOSTRA_POSTERIORI" 
#       dbWriteTable(conexao, tabelaBD,am, append=TRUE,row.names=FALSE, overwrite=FALSE,header=TRUE)
# 
#       FechaConexaoBDOracle( conexao)
      
    }
  }
}

RetNomeArq <- function (palgoritmosModelos, pNVertices, ptipoGerador, pTipoImpressao ) {
  valgoritmoant <- "inicio"
  vmodeloant <- "inicio"
  valgoritmo <- paste(tipoProcessamento,"_")
  for (i in 1:length(palgoritmosModelos)) {
   valgoritmoatual <- paste(palgoritmosModelos[[i]][1],"_",sep="")
   vmodeloatual <- paste(palgoritmosModelos[[i]][2],"_",sep="")
   if (valgoritmoatual != valgoritmoant) {
     valgoritmoant <- valgoritmoatual
     if (vmodeloatual != vmodeloant) {    
        vmodeloant <- vmodeloatual
     }
     valgoritmo <- paste(valgoritmo,valgoritmoant,vmodeloant,sep="")
   } else {
     valgoritmo <- paste(valgoritmo,vmodeloatual,sep="")
   }
  }
  setwd(config$diretorios$pdfDir)
  noFile <- paste(valgoritmo,pNVertices,"_",gsub("-","_",gsub(":", "_", dataArquivo, perl=TRUE),perl=TRUE),".pdf",sep="")
  noFile
}

maxXParam1 <- -10
minXParam1 <-  10
maxYParam1 <- -10
minYParam1 <-  10

maxXParam2 <- -10
minXParam2 <-  10
maxYParam2 <- -10
minYParam2 <-  10

j <- 1
i <- 1
for (j in 1:length(listaNVertices)) {
  pNVertices <- listaNVertices[j]
  for (i in 1:length(algoritmosModelos) ) {
    pAlgoritmo  <- algoritmosModelos[[i]][1]
    pModelo    <- algoritmosModelos[[i]][2]
    if (pAlgoritmo == "ABC_sequencial_2thetas") {
        pModel <- pSeq_model
        pPrior <- pLenormand_prior
        if (pModelo == "Lenormand") {
          pNb_simul            <- pLenormand_nb_simul       
          valgoritmo <- paste(tipoProcessamento,"_",pAlgoritmo,"_",pModelo,"_",sep="")
          setwd(config$diretorios$pdfDir)
          nomeArq <- paste(valgoritmo,pNVertices,"_",gsub("-","_",gsub(":", "_", dataArquivo, perl=TRUE),perl=TRUE),".log",sep="")
          logFile <- file(nomeArq, "w")
          cat("Data ," ,
              "nb_simul , ",
              "inside_prior , ",
              "p_acc_min, ",
              "alpha \n ",
              gsub("-","_",gsub(":", "_", dataArquivo, perl=TRUE),perl=TRUE), " , ", 
              pLenormand_nb_simul, " , ",
              pLenormand_inside_prior, " , ",
              pLenormand_p_acc_min, " , ",
              pLenormand_alpha, " , ",
              file = logFile
          )
          close(logFile)
        }
    } else if (pAlgoritmo == "Bergm_2thetas") {
        pModel     <- NULL
        pPrior     <- NULL
        pNb_simul  <- pBergm_main.iters       
        valgoritmo <- paste(tipoProcessamento,"_",pAlgoritmo,"_",pModelo,"_",sep="")
        setwd(config$diretorios$pdfDir)
        nomeArq <- paste(valgoritmo,pNVertices,"_",gsub("-","_",gsub(":", "_", dataArquivo, perl=TRUE),perl=TRUE),".log",sep="")
        logFile <- file(nomeArq, "w")
        cat("Data ," ,
            "main.iters , ",
            "burn.in , ",
            "aux.iters, ",
            "m.prior , ",
            "sigma.prior , ",
            "pBergm_nchains , ",
            "pBergm_gamma , " ,
            "pBergm_sigma.epsilon \n ",
            gsub("-","_",gsub(":", "_", dataArquivo, perl=TRUE),perl=TRUE), " , ", 
            pBergm_main.iters, " , ",
            pBergm_burn.in, " , ",
            pBergm_aux.iters, " , ",
            pBergm_m.prior, " , ",
            pBergm_sigma.prior, " , ",
            pBergm_nchains, " , ",
            pBergm_gamma, " , " ,
            pBergm_sigma.epsilon , " , ",
            file = logFile
        )
        close(logFile)
    }
    amostraAux <- GeraAmostra_APosteriori_Modelo(pGrafoObs,
                                                 pNumGrafosGerar, 
                                                 pNVertices, 
                                                 pTheta1, 
                                                 pProb,
                                                 pAlgoritmo, 
                                                 pModelo,
                                                 pModel,
                                                 pPrior, 
                                                 pNb_simul  ,
                                                 pSummary_stat_target,
                                                 pPrior_test,
                                                 pTol,
                                                 pN_cluster ,
                                                 pUse_seed  ,
                                                 pSeed_count ,
                                                 pVerbose   ,
                                                 pdist_weights,
                                                 pProgress_bar,
                                                 pDelMoral_alpha, 
                                                 pDelMoral_tolerance_target, 
                                                 pDelMoral_M, 
                                                 pDelMoral_nb_threshold,
                                                 pBeaumont_inside_prior,
                                                 pBeaumont_tolerance_tab,
                                                 pLenormand_inside_prior,
                                                 pLenormand_p_acc_min,
                                                 pLenormand_alpha,
                                                 pDrovandi_tolerance_tab,
                                                 pDrovandi_c,
                                                 pDrovandi_first_tolerance_level_auto,
                                                 pDrovandi_alpha,
                                                 pBergm_familia
                                                )
    if (!is.null(amostraAux)) {
      if (   pAlgoritmo == "ABC_sequencial_2thetas") {
        amostra[[i]] <- list(algoritmosModelos[[i]],amostraAux,mean(amostraAux$param[,1]),mean(amostraAux$param[,2]))
      } else if (pAlgoritmo == "Bergm_2thetas") {
        amostra[[i]] <- list(algoritmosModelos[[i]],amostraAux,mean(amostraAux$Theta[,1,]),mean(amostraAux$Theta[,2,]))
      } 
    }
    algoritmoMetodoAux <- amostra[[i]][1]
    algoritmoResultado <- amostra[[i]][2]
    if (!is.null(algoritmoMetodoAux)) {
      if (   algoritmoMetodoAux[[1]][1] == "ABC_sequencial_2thetas") {
        amostraTheta1     <- algoritmoResultado[[1]]$param[,1]
        amostraTheta2     <- algoritmoResultado[[1]]$param[,2]
      } else if (algoritmoMetodoAux[[1]][1] == "Bergm_2thetas"){
        amostraTheta1 <- apply(algoritmoResultado[[1]]$Theta[,1,],MARGIN = 1, mean)
        amostraTheta2 <- apply(algoritmoResultado[[1]]$Theta[,2,],MARGIN = 1, mean)
      }
      maxXParamAmostra <- matrix(0,nrow=1,ncol = 2,byrow=TRUE)
      minXParamAmostra <- matrix(0,nrow=1,ncol = 2,byrow=TRUE)
      maxXParamAmostra[1] <- max(amostraTheta1)
      minXParamAmostra[1] <- min(amostraTheta1)
      if (maxXParam1 < maxXParamAmostra[1]) {
          maxXParam1 <- maxXParamAmostra[1]
      }
      if (minXParam1 > minXParamAmostra[1]) {
          minXParam1 <- minXParamAmostra[1]
      }
      maxXParamAmostra[2] <- max(amostraTheta2)
      minXParamAmostra[2] <- min(amostraTheta2)
      if (maxXParam2 < maxXParamAmostra[2]) {
          maxXParam2 <- maxXParamAmostra[2]
      }
      if (minXParam2 > minXParamAmostra[2]) {
          minXParam2 <- minXParamAmostra[2]
      }
      amostraTeorica <- rbeta(length(amostraTheta1),alphaTeorica,betaTeorica)
      medidaKL1[i]    <- KL.plugin(amostraTeorica,amostraTheta1)
      KSTeste1[[i]]   <- ks.test(amostraTeorica,amostraTheta1)
      CHTeste1[i]     <- chi2.plugin(amostraTeorica,amostraTheta1)
      tamAmostras1[i] <- length(amostraTheta1)
    
      medidaKL2[i]    <- KL.plugin(amostraTeorica,amostraTheta2)
      KSTeste2[[i]]   <- ks.test(amostraTeorica,amostraTheta2)
      CHTeste2[i]     <- chi2.plugin(amostraTeorica,amostraTheta2)
      tamAmostras2[i] <- length(amostraTheta2)
    }
  }
  noFile <- RetNomeArq(algoritmosModelos,pNVertices,tipoGerador, pTipoImpressao = "Coda")
  pdf(width=12, height=12,file=noFile)
  maxLag <- 100
  plotResultadosCoda(amostra, pNVertices, maxXParam1, minXParam1, maxLag, medidaKL1, KSTeste1, CHTeste1,
                                          maxXParam2, minXParam2, medidaKL2, KSTeste2, CHTeste2)
  dev.off()
}

# par(mfrow=c(1,2))
# ABC_sequencial_optimize <- amostra
# vtheta1 <- ABC_sequencial_optimize[[1]][2][[1]]$param[,1]
# hist(vtheta1, freq=FALSE,main = paste("Dist. a posterior de theta1 obs = 0.2. \nMédia a posteriori : ", round(mean(vtheta1),2), "\nABC Seq Lenormand. 50 Vértices"), xlab="Theta 1")
# lines(density(vtheta1)$x,density(vtheta1)$y, col="red", lwd=2)
# 
# vtheta2 <- ABC_sequencial_optimize[[1]][2][[1]]$param[,2]
# hist(vtheta2, freq=FALSE,main = paste("Dist. a posterior de theta2 obs = 0.2. \nMédia a posteriori : ", round(mean(vtheta2),2), "\nABC Seq Lenormand. 50 Vértices"), xlab="Theta 1")
# lines(density(vtheta2)$x,density(vtheta2)$y, col="green",lwd=2)

