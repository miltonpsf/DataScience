library("entropy")
rm(list=ls())
nVertices      <- matrix(c(50,100,200,500,750,1000,1250,1500,1750,2000,2250),1,11)
alphaBetaTeorica <- matrix (c(891,335,
                                3615,1336,
                                14569,5332,
                                91129,33622,
                                205218,75658,
                                364981,134520,
                                570540,210086,
                                821247,303004,
                                1118197,412179,
                                1461186,537815,
                                1849227,680899),2,11)
rownames(alphaBetaTeorica) <- c("alphaTeorica","betaTeorica")
colnames(alphaBetaTeorica) <- nVertices
resultadoKL    <- matrix(1,11,1000)
resultadoKS    <- matrix(1,11,1000)
resultadoChi2  <- matrix(1,11,1000)
i <- 1
j <- 1
for (i in 1:11) {
  for (j in 1:1000) {
    amostraTeorica1     <- rbeta(1000,alphaBetaTeorica[1,i],alphaBetaTeorica[2,i])
    amostraTeorica2     <- rbeta(1000,alphaBetaTeorica[1,i],alphaBetaTeorica[2,i])
    resultadoKL[i,j]   <- KL.plugin(amostraTeorica1, amostraTeorica2)
    resultadoKS[i,j]   <- (ks.test(amostraTeorica1,amostraTeorica2))$statistic
    resultadoChi2[i,j] <- chi2.plugin(amostraTeorica1,amostraTeorica2)
  }
}  

intervaloConfKL <- matrix(0,11,2)
intervaloConfKS <- matrix(0,11,2)
intervaloConfChi2 <- matrix(0,11,2)
nomeColunas  <- matrix(NA,11,1)
for (k in 1:11) {
#  nomeColunas[k]  <- paste(nVertices[k],"-",alphaBetaTeorica[1,k],"-",alphaBetaTeorica[2,k],sep="")
  nomeColunas[k]  <- paste(nVertices[k],sep="")
  intervaloConfKL[k,] <- quantile(resultadoKL[k,],probs=c(0.95,0.99))
  intervaloConfKS[k,] <- quantile(resultadoKS[k,],probs=c(0.95,0.99))
  intervaloConfChi2[k,] <- quantile(resultadoChi2[k,],probs=c(0.95,0.99))
}

rownames(intervaloConfKL)    <- nomeColunas
colnames(intervaloConfKL)    <- c("qtile 95% KL","qtile 99% KL")
rownames(intervaloConfKS)    <- nomeColunas
colnames(intervaloConfKS)    <- c("qtile 95% KS","qtile 99% KS")
rownames(intervaloConfChi2)  <- nomeColunas
colnames(intervaloConfChi2)  <- c("qtile 95% Chi2","qtile 99% Chi2")
intervaloConfKL
intervaloConfKS
intervaloConfChi2

write.csv(intervaloConfKL,"intervaloConfKL.csv")
write.csv(intervaloConfKS,"intervaloConfKS.csv")
write.csv(intervaloConfChi2,"intervaloConfChi2.csv")

# alphaTeorica <- 891
# BetaTeorica  <- 335
# amostraTeorica <- rbeta(1000,alphaTeorica,BetaTeorica)
# resultadoKS <- (ks.test(amostraTeorica,amostraTeorica))$statistic
# resultadoKS$statistic
#  
# # probabilities for two random variables
# freqs1 = c(1/5, 1/5, 3/5)
# freqs2 = c(1/10, 4/10, 1/2) 
# freqs2 = c(1/5, 1/5, 3/5) 
#  
# # KL divergence from X1 to X2
# KL.plugin(freqs1, freqs2)
# chi2.plugin(freqs1, freqs2)

