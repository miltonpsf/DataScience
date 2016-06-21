rm(list=ls())
library(shiny)
library(gdata)
library(sand)
library(coda)
library(reshape)
library("entropy")
library("yaml")
library("RJDBC")
library("ggplot2")
library("PerformanceAnalytics")
library(gridExtra)
myDir<- paste0(path.expand("~"),"/DataScience/ImplementacoesABC_Defesa")
configFile <- paste(myDir, "config.yml", sep="/");
config <- yaml.load_file(configFile)
setwd(config$diretorios$workDir)
source("FuncoesAcessaDadosBD.R")

intervaloConfKL   <- read.csv("intervaloConfKL.csv")
names(intervaloConfKL) <- c("NU_VERTICES","KL95","KL99")
intervaloConfKS   <- read.csv("intervaloConfKS.csv")
names(intervaloConfKS) <- c("NU_VERTICES","KS95","KS99")
intervaloConfChi2 <- read.csv("intervaloConfChi2.csv")
names(intervaloConfChi2) <- c("NU_VERTICES","CHI95","CHI99")

#conexao  <- RetornaConexaoBDOracle( config )
#dadosResultado <- RetornaResultadoAlgoritmos( conexao)
#lagAlgoritmos <- RetornaLagAlgoritmos( conexao)
#save(dadosResultado, file="dadosResultado.RData")
#save(lagAlgoritmos, file="lagAlgoritmos.RData")
load("dadosResultado.RData")
load("lagAlgoritmos.RData")

dadosResultadoBergm50  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 50)
dadosResultadoBergm100  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 100)
dadosResultadoBergm200  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 200)
dadosResultadoBergm500  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 500)
dadosResultadoBergm750  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 750)
dadosResultadoBergm1000  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 1000)
dadosResultadoBergm1250  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 1250)
dadosResultadoBergm1500  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 1500)
dadosResultadoBergm1750  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 1750)
dadosResultadoBergm2000  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 2000)
dadosResultadoBergm2250  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == 2250)
 
# amostraBergm50 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm50$DT_PROCESSAMENTO)
# save(amostraBergm50, file="amostraBergm50.RData")
# amostraBergm100 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm100$DT_PROCESSAMENTO)
# save(amostraBergm100, file="amostraBergm100.RData")
# amostraBergm200 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm200$DT_PROCESSAMENTO)
# save(amostraBergm200, file="amostraBergm200.RData")
# amostraBergm500 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm500$DT_PROCESSAMENTO)
# save(amostraBergm500, file="amostraBergm500.RData")
# amostraBergm750 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm750$DT_PROCESSAMENTO)
# save(amostraBergm750, file="amostraBergm750.RData")
# amostraBergm1000 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm1000$DT_PROCESSAMENTO)
# save(amostraBergm1000, file="amostraBergm1000.RData")
# amostraBergm1250 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm1250$DT_PROCESSAMENTO)
# save(amostraBergm1250, file="amostraBergm1250.RData")
# amostraBergm1500 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm1500$DT_PROCESSAMENTO)
# save(amostraBergm1500, file="amostraBergm1500.RData")
# amostraBergm1750 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm1750$DT_PROCESSAMENTO)
# save(amostraBergm1750, file="amostraBergm1750.RData")
# amostraBergm2000 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm2000$DT_PROCESSAMENTO)
# save(amostraBergm2000, file="amostraBergm2000.RData")
# amostraBergm2250 <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm2250$DT_PROCESSAMENTO)
# save(amostraBergm2250, file="amostraBergm2250.RData")

# modelo <- "Delmoral"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral50  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral50, file="dadosResultadoABCDelmoral50.RData")
# 
# amostraABCDelmoral50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral50, file="amostraABCDelmoral50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral100  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral100, file="dadosResultadoABCDelmoral100.RData")
# 
# amostraABCDelmoral100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral100, file="amostraABCDelmoral100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral200  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral200, file="dadosResultadoABCDelmoral200.RData")
# 
# amostraABCDelmoral200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral200, file="amostraABCDelmoral200.RData")
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral500  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral500, file="dadosResultadoABCDelmoral500.RData")
# 
# amostraABCDelmoral500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral500, file="amostraABCDelmoral500.RData")
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral750  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral750, file="dadosResultadoABCDelmoral750.RData")
# 
# amostraABCDelmoral750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral750, file="amostraABCDelmoral750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral1000  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral1000, file="dadosResultadoABCDelmoral1000.RData")
# 
# amostraABCDelmoral1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral1000, file="amostraABCDelmoral1000.RData")
# 
# nVertices <- 1250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral1250  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral1250, file="dadosResultadoABCDelmoral1250.RData")
# 
# amostraABCDelmoral1250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral1250, file="amostraABCDelmoral1250.RData")
# 
# nVertices <- 1500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral1500  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral1500, file="dadosResultadoABCDelmoral1500.RData")
# 
# amostraABCDelmoral1500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral1500, file="amostraABCDelmoral1500.RData")
# 
# nVertices <- 1750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral1750  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral1750, file="dadosResultadoABCDelmoral1750.RData")
# 
# amostraABCDelmoral1750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral1750, file="amostraABCDelmoral1750.RData")
# 
# nVertices <- 2000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral2000  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral2000, file="dadosResultadoABCDelmoral2000.RData")
# 
# amostraABCDelmoral2000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral2000, file="amostraABCDelmoral2000.RData")
# 
# nVertices <- 2250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDelmoral2250  <- dadosResultadoABC
# save(dadosResultadoABCDelmoral2250, file="dadosResultadoABCDelmoral2250.RData")
# 
# amostraABCDelmoral2250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDelmoral2250, file="amostraABCDelmoral2250.RData")

# modelo <- "Beaumont"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont50  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont50, file="dadosResultadoABCBeaumont50.RData")
# 
# amostraABCBeaumont50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont50, file="amostraABCBeaumont50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont100  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont100, file="dadosResultadoABCBeaumont100.RData")
# 
# amostraABCBeaumont100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont100, file="amostraABCBeaumont100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont200  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont200, file="dadosResultadoABCBeaumont200.RData")
# 
# amostraABCBeaumont200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont200, file="amostraABCBeaumont200.RData")
# 
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont500  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont500, file="dadosResultadoABCBeaumont500.RData")
# 
# amostraABCBeaumont500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont500, file="amostraABCBeaumont500.RData")
# 
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont750  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont750, file="dadosResultadoABCBeaumont750.RData")
# 
# amostraABCBeaumont750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont750, file="amostraABCBeaumont750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont1000  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont1000, file="dadosResultadoABCBeaumont1000.RData")
# 
# amostraABCBeaumont1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont1000, file="amostraABCBeaumont1000.RData")
# 
# nVertices <- 1250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont1250  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont1250, file="dadosResultadoABCBeaumont1250.RData")
# 
# amostraABCBeaumont1250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont1250, file="amostraABCBeaumont1250.RData")
# 
# nVertices <- 1500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont1500  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont1500, file="dadosResultadoABCBeaumont1500.RData")
# 
# amostraABCBeaumont1500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont1500, file="amostraABCBeaumont1500.RData")
# 
# nVertices <- 1750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont1750  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont1750, file="dadosResultadoABCBeaumont1750.RData")
# 
# amostraABCBeaumont1750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont1750, file="amostraABCBeaumont1750.RData")
# 
# nVertices <- 2000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont2000  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont2000, file="dadosResultadoABCBeaumont2000.RData")
# 
# amostraABCBeaumont2000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont2000, file="amostraABCBeaumont2000.RData")
# 
# nVertices <- 2250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCBeaumont2250  <- dadosResultadoABC
# save(dadosResultadoABCBeaumont2250, file="dadosResultadoABCBeaumont2250.RData")
# 
# amostraABCBeaumont2250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCBeaumont2250, file="amostraABCBeaumont2250.RData")
# 
# 
# modelo <- "Lenormand"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand50  <- dadosResultadoABC
# save(dadosResultadoABCLenormand50, file="dadosResultadoABCLenormand50.RData")
# 
# amostraABCLenormand50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand50, file="amostraABCLenormand50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand100  <- dadosResultadoABC
# save(dadosResultadoABCLenormand100, file="dadosResultadoABCLenormand100.RData")
# 
# amostraABCLenormand100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand100, file="amostraABCLenormand100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand200  <- dadosResultadoABC
# save(dadosResultadoABCLenormand200, file="dadosResultadoABCLenormand200.RData")
# 
# amostraABCLenormand200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand200, file="amostraABCLenormand200.RData")
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand500  <- dadosResultadoABC
# save(dadosResultadoABCLenormand500, file="dadosResultadoABCLenormand500.RData")
# 
# amostraABCLenormand500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand500, file="amostraABCLenormand500.RData")
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand750  <- dadosResultadoABC
# save(dadosResultadoABCLenormand750, file="dadosResultadoABCLenormand750.RData")
# 
# amostraABCLenormand750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand750, file="amostraABCLenormand750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand1000  <- dadosResultadoABC
# save(dadosResultadoABCLenormand1000, file="dadosResultadoABCLenormand1000.RData")
# 
# amostraABCLenormand1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand1000, file="amostraABCLenormand1000.RData")
# 
# nVertices <- 1250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand1250  <- dadosResultadoABC
# save(dadosResultadoABCLenormand1250, file="dadosResultadoABCLenormand1250.RData")
# 
# amostraABCLenormand1250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand1250, file="amostraABCLenormand1250.RData")
# 
# nVertices <- 1500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand1500  <- dadosResultadoABC
# save(dadosResultadoABCLenormand1500, file="dadosResultadoABCLenormand1500.RData")
# 
# amostraABCLenormand1500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand1500, file="amostraABCLenormand1500.RData")
# 
# nVertices <- 1750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand1750  <- dadosResultadoABC
# save(dadosResultadoABCLenormand1750, file="dadosResultadoABCLenormand1750.RData")
# 
# amostraABCLenormand1750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand1750, file="amostraABCLenormand1750.RData")
# 
# nVertices <- 2000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand2000  <- dadosResultadoABC
# save(dadosResultadoABCLenormand2000, file="dadosResultadoABCLenormand2000.RData")
# 
# amostraABCLenormand2000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand2000, file="amostraABCLenormand2000.RData")
# 
# nVertices <- 2250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCLenormand2250  <- dadosResultadoABC
# save(dadosResultadoABCLenormand2250, file="dadosResultadoABCLenormand2250.RData")
# 
# amostraABCLenormand2250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCLenormand2250, file="amostraABCLenormand2250.RData")
# 
# modelo <- "Drovandi"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi50  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi50, file="dadosResultadoABCDrovandi50.RData")
# 
# amostraABCDrovandi50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi50, file="amostraABCDrovandi50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi100  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi100, file="dadosResultadoABCDrovandi100.RData")
# 
# amostraABCDrovandi100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi100, file="amostraABCDrovandi100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi200  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi200, file="dadosResultadoABCDrovandi200.RData")
# 
# amostraABCDrovandi200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi200, file="amostraABCDrovandi200.RData")
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi500  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi500, file="dadosResultadoABCDrovandi500.RData")
# 
# amostraABCDrovandi500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi500, file="amostraABCDrovandi500.RData")
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi750  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi750, file="dadosResultadoABCDrovandi750.RData")
# 
# amostraABCDrovandi750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi750, file="amostraABCDrovandi750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi1000  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi1000, file="dadosResultadoABCDrovandi1000.RData")
# 
# amostraABCDrovandi1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi1000, file="amostraABCDrovandi1000.RData")
# 
# nVertices <- 1250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi1250  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi1250, file="dadosResultadoABCDrovandi1250.RData")
# 
# amostraABCDrovandi1250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi1250, file="amostraABCDrovandi1250.RData")
# 
# nVertices <- 1500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi1500  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi1500, file="dadosResultadoABCDrovandi1500.RData")
# 
# amostraABCDrovandi1500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi1500, file="amostraABCDrovandi1500.RData")
# 
# nVertices <- 1750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi1750  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi1750, file="dadosResultadoABCDrovandi1750.RData")
# 
# amostraABCDrovandi1750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi1750, file="amostraABCDrovandi1750.RData")
# 
# nVertices <- 2000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi2000  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi2000, file="dadosResultadoABCDrovandi2000.RData")
# 
# amostraABCDrovandi2000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi2000, file="amostraABCDrovandi2000.RData")
# 
# nVertices <- 2250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCDrovandi2250  <- dadosResultadoABC
# save(dadosResultadoABCDrovandi2250, file="dadosResultadoABCDrovandi2250.RData")
# 
# amostraABCDrovandi2250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCDrovandi2250, file="amostraABCDrovandi2250.RData")
# 
# modelo <- "Rejeicao"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao50  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao50, file="dadosResultadoABCRejeicao50.RData")
# 
# amostraABCRejeicao50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao50, file="amostraABCRejeicao50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao100  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao100, file="dadosResultadoABCRejeicao100.RData")
# 
# amostraABCRejeicao100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao100, file="amostraABCRejeicao100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao200  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao200, file="dadosResultadoABCRejeicao200.RData")
# 
# amostraABCRejeicao200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao200, file="amostraABCRejeicao200.RData")
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao500  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao500, file="dadosResultadoABCRejeicao500.RData")
# 
# amostraABCRejeicao500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao500, file="amostraABCRejeicao500.RData")
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao750  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao750, file="dadosResultadoABCRejeicao750.RData")
# 
# amostraABCRejeicao750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao750, file="amostraABCRejeicao750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao1000  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao1000, file="dadosResultadoABCRejeicao1000.RData")
# 
# amostraABCRejeicao1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao1000, file="amostraABCRejeicao1000.RData")
# 
# nVertices <- 1250
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCRejeicao1250  <- dadosResultadoABC
# save(dadosResultadoABCRejeicao1250, file="dadosResultadoABCRejeicao1250.RData")
# 
# amostraABCRejeicao1250 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCRejeicao1250, file="amostraABCRejeicao1250.RData")
# 
# modelo <- "Marjoram"
# nVertices <- 50
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram50  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram50, file="dadosResultadoABCMarjoram50.RData")
# 
# amostraABCMarjoram50 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram50, file="amostraABCMarjoram50.RData")
# 
# nVertices <- 100
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram100  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram100, file="dadosResultadoABCMarjoram100.RData")
# 
# amostraABCMarjoram100 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram100, file="amostraABCMarjoram100.RData")
# 
# nVertices <- 200
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram200  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram200, file="dadosResultadoABCMarjoram200.RData")
# 
# amostraABCMarjoram200 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram200, file="amostraABCMarjoram200.RData")
# 
# nVertices <- 500
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram500  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram500, file="dadosResultadoABCMarjoram500.RData")
# 
# amostraABCMarjoram500 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram500, file="amostraABCMarjoram500.RData")
# 
# nVertices <- 750
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram750  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram750, file="dadosResultadoABCMarjoram750.RData")
# 
# amostraABCMarjoram750 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram750, file="amostraABCMarjoram750.RData")
# 
# nVertices <- 1000
# dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
# dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
# dadosResultadoABCMarjoram1000  <- dadosResultadoABC
# save(dadosResultadoABCMarjoram1000, file="dadosResultadoABCMarjoram1000.RData")
# 
# amostraABCMarjoram1000 <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
# save(amostraABCMarjoram1000, file="amostraABCMarjoram1000.RData")

#write.csv(dadosResultado,"dadosResultado.csv")

retornaDadosResutadoPorV <- function (dadosResultado, pNumVertice, pIntervaloConfKL, pIntervaloConfKS, pIntervaloConfChi2) {
  dadosResultadoPorV <- subset(dadosResultado, NU_VERTICES == pNumVertice)
#  dadosResultadoPorV  <- dadosResultadoPorV[,-c(1,2,4,5,7,9,10,16,17,18)]
  dadosResultadoPorV  <- dadosResultadoPorV[,-c(1,2,4,5,7,9,10,14,16,17,18)]
#  names(dadosResultadoPorV)  <- c("Modelo","Media posteriori","Vertice","KL","KS","KS-pvalue","ChiQ","ESS")
  names(dadosResultadoPorV)  <- c("Modelo","Media posteriori","Vertice","KL","KS","KS-pvalue","ESS")
#  dadosResultadoPorV <- dadosResultadoPorV[c("Vertice","Modelo","Media posteriori","KL","KS","KS-pvalue","ChiQ","ESS")]
  dadosResultadoPorV <- dadosResultadoPorV[c("Vertice","Modelo","Media posteriori","KL","KS","KS-pvalue","ESS")]
  intervaloConfKLPorV <- subset(pIntervaloConfKL, NU_VERTICES == pNumVertice)   
  intervaloConfKSPorV <- subset(pIntervaloConfKS, NU_VERTICES == pNumVertice)   
  intervaloConfChiPorV <- subset(pIntervaloConfChi2, NU_VERTICES == pNumVertice)   
  intervaloConfKLPorV <- intervaloConfKLPorV[,-1]
  intervaloConfKSPorV <- intervaloConfKSPorV[,-1]
#  intervaloConfChiPorV <- intervaloConfChiPorV[,-1]
  intervaloConfPValuePorV <- as.data.frame(t(c(0.1,0.05)))
  names(intervaloConfPValuePorV) <- c("PV10","PV05")
#  dadosResultadoPorV <- cbind(dadosResultadoPorV,intervaloConfKLPorV,intervaloConfKSPorV,intervaloConfChiPorV,intervaloConfPValuePorV)
  dadosResultadoPorV <- cbind(dadosResultadoPorV,intervaloConfKLPorV,intervaloConfKSPorV,intervaloConfPValuePorV)
  attach(dadosResultadoPorV)
  dadosResultadoPorV <- dadosResultadoPorV[order(Modelo),]
  return(dadosResultadoPorV)
}
dadosResultado50   <- retornaDadosResutadoPorV(dadosResultado,50,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado100  <- retornaDadosResutadoPorV(dadosResultado,100,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado200  <- retornaDadosResutadoPorV(dadosResultado,200,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado500  <- retornaDadosResutadoPorV(dadosResultado,500,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado750  <- retornaDadosResutadoPorV(dadosResultado,750,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado1000 <- retornaDadosResutadoPorV(dadosResultado,1000,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado1250 <- retornaDadosResutadoPorV(dadosResultado,1250,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado1500 <- retornaDadosResutadoPorV(dadosResultado,1500,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado1750 <- retornaDadosResutadoPorV(dadosResultado,1750,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado2000 <- retornaDadosResutadoPorV(dadosResultado,2000,intervaloConfKL, intervaloConfKS, intervaloConfChi2)
dadosResultado2250 <- retornaDadosResutadoPorV(dadosResultado,2250,intervaloConfKL, intervaloConfKS, intervaloConfChi2)

# Define server logic for random distribution application
shinyServer(function(input, output, session) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    modelo <- switch(input$modelo,
                   Delmoral = Delmoral,
                   Beaumont = Beaumont,
                   Lenormand = Lenormand,
                   Drovandi = Drovandi,
                   Rejeicao = Rejeicao,
                   Marjoram = Marjoram
                   )
    
    nVertices <- switch(input$selecionaVertices)
    
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$BergmG <- renderPlot({
    
    modelo <- input$modelo
    nVertices <- input$selecionaVertices
    if (nVertices == 50) {
      load("amostraBergm50.RData")
      amostraBergm <- amostraBergm50
      dadosResultadoBergm <- dadosResultadoBergm50
    }
    if (nVertices == 100) {
      load("amostraBergm100.RData")
      amostraBergm <- amostraBergm100
      dadosResultadoBergm <- dadosResultadoBergm100
    }
    if (nVertices == 200) {
      load("amostraBergm200.RData")
      amostraBergm <- amostraBergm200
      dadosResultadoBergm <- dadosResultadoBergm200
    }
    if (nVertices == 500) {
      load("amostraBergm500.RData")
      amostraBergm <- amostraBergm500
      dadosResultadoBergm <- dadosResultadoBergm500
    }
    if (nVertices == 750) {
      load("amostraBergm750.RData")
      amostraBergm <- amostraBergm750
      dadosResultadoBergm <- dadosResultadoBergm750
    }
    if (nVertices == 1000) {
      load("amostraBergm1000.RData")
      amostraBergm <- amostraBergm1000
      dadosResultadoBergm <- dadosResultadoBergm1000
    }
    if (nVertices == 1250) {
      load("amostraBergm1250.RData")
      amostraBergm <- amostraBergm1250
      dadosResultadoBergm <- dadosResultadoBergm1250
    }
    if (nVertices == 1500) {
      load("amostraBergm1500.RData")
      amostraBergm <- amostraBergm1500
      dadosResultadoBergm <- dadosResultadoBergm1500
    }
    if (nVertices == 1750) {
      load("amostraBergm1750.RData")
      amostraBergm <- amostraBergm1750
      dadosResultadoBergm <- dadosResultadoBergm1750
    }
    if (nVertices == 2000) {
      load("amostraBergm2000.RData")
      amostraBergm <- amostraBergm2000
      dadosResultadoBergm <- dadosResultadoBergm2000
    }
    if (nVertices == 2250) {
      load("amostraBergm2250.RData")
      amostraBergm <- amostraBergm2250
      dadosResultadoBergm <- dadosResultadoBergm2250
    }
    #dadosResultadoBergm  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == nVertices)
    #amostraBergm <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm$DT_PROCESSAMENTO)
    #write.csv(amostraBergm,paste("amostraBergm_",nVertices,".csv",sep=""))
    
    par(mfrow=c(1,4))
    pminXParam <- min(amostraBergm$VL_AMOSTRA_THETA)
    pmaxXParam <- max(amostraBergm$VL_AMOSTRA_THETA)

    lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO =="Bergm" & NU_VERTICES == nVertices)
    lag <- lagAlgoritmos$LAG
    T <- NROW(amostraBergm)
    theta1 <- amostraBergm$VL_AMOSTRA_THETA
    ind <- seq(lag,T,by=lag)
    theta1 <- theta1[ind]
    T <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)

    amostraMCMC <- mcmc(theta1)

    alphaTeorica <- dadosResultadoBergm$ALPHA_TEORICA
    betaTeorica <-  dadosResultadoBergm$BETA_TEORICA 
    denAmostraTheta <- density(amostraBergm$VL_AMOSTRA_THETA)
    maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
    densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      #xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste("Theta1 = ",dadosResultadoBergm$VL_THETA1,
                                      " , Prob = ",round(as.numeric(dadosResultadoBergm$VL_PROBABILIDADE),3),
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoBergm$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoBergm$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoBergm$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )

      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoBergm$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoBergm$KS_TEST_EST,4),", p-value :",round(dadosResultadoBergm$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoBergm$CHISQ_TEST_EST,4), sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoBergm$VL_MEDIA_POSTERIORI),3)
      ##abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoBergm$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoBergm$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
  })

#   output$BergmI <- renderPlot({
#     modelo <- input$modelo
#     nVertices <- input$selecionaVertices
#     dadosResultadoBergm  <- subset(dadosResultado, NO_MODELO =="Bergm" & NU_VERTICES == nVertices)
#     amostraBergm <- RetornaAmostraAlgoritmos(conexao, "Bergm", dadosResultadoBergm$DT_PROCESSAMENTO)
# 
#     par(mfrow=c(1,3))
#     pminXParam <- min(amostraBergm$VL_AMOSTRA_THETA)
#     pmaxXParam <- max(amostraBergm$VL_AMOSTRA_THETA)
#     amostraMCMC <- mcmc(amostraBergm$VL_AMOSTRA_THETA)
#     alphaTeorica <- dadosResultadoBergm$ALPHA_TEORICA
#     betaTeorica <-  dadosResultadoBergm$BETA_TEORICA 
#     
#     denAmostraTheta <- density(amostraBergm$VL_AMOSTRA_THETA)
#     maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
#                             dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
#     densplot(amostraMCMC,type="l", lwd=2,
#                                       xlab="Probabilidade",
#                                       ylab="T=Azul e A=Preta",
#                                       #xlim=c(pminXParam,pmaxXParam),
#                                       ylim=c(0,maxYParam),
#                                       main=paste("Theta1 = ",dadosResultadoBergm$VL_THETA1,
#                                       " , Prob = ",round(as.numeric(dadosResultadoBergm$VL_PROBABILIDADE),3),
#                                       "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoBergm$VL_MEDIA_POSTERIORI),3),
#                                       "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoBergm$VL_MEDIA_TEORICA),3),
#                                       ", Tempo = ", dadosResultadoBergm$NU_SEG_PROCESSAMENTO," s",sep="")
#                                       )
#       pmaxLag <- 100
#       curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
#       traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoBergm$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoBergm$KS_TEST_EST,4),", p-value :",round(dadosResultadoBergm$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoBergm$CHISQ_TEST_EST,4), sep=""))
# #     autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoBergm$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoBergm$NEFF,sep=""))
#       acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoBergm$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoBergm$NEFF,sep=""))
#   })
    
#   output$ABC <- renderPlot({
#     
#     modelo <- input$modelo
#     nVertices <- input$selecionaVertices
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#     write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))
# 
#     par(mfrow=c(1,4))
#     pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
#     pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)
#     amostraMCMC <- mcmc(amostraABC$VL_AMOSTRA_THETA)
#     alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
#     betaTeorica <-  dadosResultadoABC$BETA_TEORICA 
# 
#     denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
#     maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
#                             dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
#     densplot(amostraMCMC,type="l", lwd=2,
#                                       xlab="Probabilidade",
#                                       ylab="T=Azul e A=Preta",
#                                       xlim=c(pminXParam,pmaxXParam),
#                                       ylim=c(0,maxYParam),
#                                       main=paste(modelo, 
#                                       "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
#                                       "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
#                                       ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
#                                       )
#       pmaxLag <- 100
#       curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
#       traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
# #      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
#       acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
#       lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#       lag <- lagAlgoritmos$LAG
#       T <- NROW(amostraABC)
#       theta1 <- amostraABC$VL_AMOSTRA_THETA
#       estTheta1 =cumsum(amostraABC$VL_AMOSTRA_THETA)/(1:T)
#       esterrTheta1=sqrt(cumsum((amostraABC$VL_AMOSTRA_THETA-estTheta1)^2))/(1:T)
#       plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
#       + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
#       lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
#       lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
#   })

  output$Delmoral <- renderPlot({
    modelos <- input$modelos
    modelo <- "Delmoral"
    nVertices <- input$selecionaVertices
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#     write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))
    
    if (nVertices == 50) {
      load("amostraABCDelmoral50.RData")
      amostraABC <- amostraABCDelmoral50
      load("dadosResultadoABCDelmoral50.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral50
    }
    if (nVertices == 100) {
      load("amostraABCDelmoral100.RData")
      amostraABC <- amostraABCDelmoral100
      load("dadosResultadoABCDelmoral100.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral100
    }
    if (nVertices == 200) {
      load("amostraABCDelmoral200.RData")
      amostraABC <- amostraABCDelmoral200
      load("dadosResultadoABCDelmoral200.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral200
    }
    if (nVertices == 500) {
      load("amostraABCDelmoral500.RData")
      amostraABC <- amostraABCDelmoral500
      load("dadosResultadoABCDelmoral500.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral500
    }
    if (nVertices == 750) {
      load("amostraABCDelmoral750.RData")
      amostraABC <- amostraABCDelmoral750
      load("dadosResultadoABCDelmoral750.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral750
    }
    if (nVertices == 1000) {
      load("amostraABCDelmoral1000.RData")
      amostraABC <- amostraABCDelmoral1000
      load("dadosResultadoABCDelmoral1000.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral1000
    }
    if (nVertices == 1250) {
      load("amostraABCDelmoral1250.RData")
      amostraABC <- amostraABCDelmoral1250
      load("dadosResultadoABCDelmoral1250.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral1250
    }
    if (nVertices == 1500) {
      load("amostraABCDelmoral1500.RData")
      amostraABC <- amostraABCDelmoral1500
      load("dadosResultadoABCDelmoral1500.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral1500
    }
    if (nVertices == 1750) {
      load("amostraABCDelmoral1750.RData")
      amostraABC <- amostraABCDelmoral1750
      load("dadosResultadoABCDelmoral1750.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral1750
    }
    if (nVertices == 2000) {
      load("amostraABCDelmoral2000.RData")
      amostraABC <- amostraABCDelmoral2000
      load("dadosResultadoABCDelmoral2000.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral2000
    }
    if (nVertices == 2250) {
      load("amostraABCDelmoral2250.RData")
      amostraABC <- amostraABCDelmoral2250
      load("dadosResultadoABCDelmoral2250.RData")
      dadosResultadoABC <- dadosResultadoABCDelmoral2250
    }

    par(mfrow=c(1,4))
    pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
    pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

    lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
    lag <- lagAlgoritmos$LAG
    T <- NROW(amostraABC)
    theta1 <- amostraABC$VL_AMOSTRA_THETA
    ind <- seq(lag,T,by=lag)
    theta1 <- theta1[ind]
    T <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)

    amostraMCMC <- mcmc(theta1)
    alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
    betaTeorica <-  dadosResultadoABC$BETA_TEORICA 
    
    denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
    maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
   densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      
      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")

  })

  output$Beaumont <- renderPlot({
    modelos <- input$modelos
    modelo <- "Beaumont"
    nVertices <- input$selecionaVertices
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#     write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))
    
    if (nVertices == 50) {
      load("amostraABCBeaumont50.RData")
      amostraABC <- amostraABCBeaumont50
      load("dadosResultadoABCBeaumont50.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont50
    }
    if (nVertices == 100) {
      load("amostraABCBeaumont100.RData")
      amostraABC <- amostraABCBeaumont100
      load("dadosResultadoABCBeaumont100.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont100
    }
    if (nVertices == 200) {
      load("amostraABCBeaumont200.RData")
      amostraABC <- amostraABCBeaumont200
      load("dadosResultadoABCBeaumont200.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont200
    }
    if (nVertices == 500) {
      load("amostraABCBeaumont500.RData")
      amostraABC <- amostraABCBeaumont500
      load("dadosResultadoABCBeaumont500.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont500
    }
    if (nVertices == 750) {
      load("amostraABCBeaumont750.RData")
      amostraABC <- amostraABCBeaumont750
      load("dadosResultadoABCBeaumont750.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont750
    }
    if (nVertices == 1000) {
      load("amostraABCBeaumont1000.RData")
      amostraABC <- amostraABCBeaumont1000
      load("dadosResultadoABCBeaumont1000.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont1000
    }
    if (nVertices == 1250) {
      load("amostraABCBeaumont1250.RData")
      amostraABC <- amostraABCBeaumont1250
      load("dadosResultadoABCBeaumont1250.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont1250
    }
    if (nVertices == 1500) {
      load("amostraABCBeaumont1500.RData")
      amostraABC <- amostraABCBeaumont1500
      load("dadosResultadoABCBeaumont1500.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont1500
    }
    if (nVertices == 1750) {
      load("amostraABCBeaumont1750.RData")
      amostraABC <- amostraABCBeaumont1750
      load("dadosResultadoABCBeaumont1750.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont1750
    }
    if (nVertices == 2000) {
      load("amostraABCBeaumont2000.RData")
      amostraABC <- amostraABCBeaumont2000
      load("dadosResultadoABCBeaumont2000.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont2000
    }
    if (nVertices == 2250) {
      load("amostraABCBeaumont2250.RData")
      amostraABC <- amostraABCBeaumont2250
      load("dadosResultadoABCBeaumont2250.RData")
      dadosResultadoABC <- dadosResultadoABCBeaumont2250
    }

    par(mfrow=c(1,4))
    pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
    pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

    lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
    lag <- lagAlgoritmos$LAG
    T <- NROW(amostraABC)
    theta1 <- amostraABC$VL_AMOSTRA_THETA
    ind <- seq(lag,T,by=lag)
    theta1 <- theta1[ind]
    T <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)
    
    amostraMCMC <- mcmc(theta1)
    alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
    betaTeorica <-  dadosResultadoABC$BETA_TEORICA 

    denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
    maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
    if (nVertices >= 1000) {
      pmaxXParam <- 0.8
    }
    densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red",n=50000)
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
  })

  output$Lenormand <- renderPlot({
    modelos <- input$modelos
    modelo <- "Lenormand"
    nVertices <- input$selecionaVertices
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#     write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))

    if (nVertices == 50) {
      load("amostraABCLenormand50.RData")
      amostraABC <- amostraABCLenormand50
      load("dadosResultadoABCLenormand50.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand50
    }
    if (nVertices == 100) {
      load("amostraABCLenormand100.RData")
      amostraABC <- amostraABCLenormand100
      load("dadosResultadoABCLenormand100.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand100
    }
    if (nVertices == 200) {
      load("amostraABCLenormand200.RData")
      amostraABC <- amostraABCLenormand200
      load("dadosResultadoABCLenormand200.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand200
    }
    if (nVertices == 500) {
      load("amostraABCLenormand500.RData")
      amostraABC <- amostraABCLenormand500
      load("dadosResultadoABCLenormand500.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand500
    }
    if (nVertices == 750) {
      load("amostraABCLenormand750.RData")
      amostraABC <- amostraABCLenormand750
      load("dadosResultadoABCLenormand750.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand750
    }
    if (nVertices == 1000) {
      load("amostraABCLenormand1000.RData")
      amostraABC <- amostraABCLenormand1000
      load("dadosResultadoABCLenormand1000.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand1000
    }
    if (nVertices == 1250) {
      load("amostraABCLenormand1250.RData")
      amostraABC <- amostraABCLenormand1250
      load("dadosResultadoABCLenormand1250.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand1250
    }
    if (nVertices == 1500) {
      load("amostraABCLenormand1500.RData")
      amostraABC <- amostraABCLenormand1500
      load("dadosResultadoABCLenormand1500.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand1500
    }
    if (nVertices == 1750) {
      load("amostraABCLenormand1750.RData")
      amostraABC <- amostraABCLenormand1750
      load("dadosResultadoABCLenormand1750.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand1750
    }
    if (nVertices == 2000) {
      load("amostraABCLenormand2000.RData")
      amostraABC <- amostraABCLenormand2000
      load("dadosResultadoABCLenormand2000.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand2000
    }
    if (nVertices == 2250) {
      load("amostraABCLenormand2250.RData")
      amostraABC <- amostraABCLenormand2250
      load("dadosResultadoABCLenormand2250.RData")
      dadosResultadoABC <- dadosResultadoABCLenormand2250
    }

    par(mfrow=c(1,4))
    pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
    pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

    lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
    lag <- lagAlgoritmos$LAG
    T <- NROW(amostraABC)
    theta1 <- amostraABC$VL_AMOSTRA_THETA
    ind <- seq(lag,T,by=lag)
    theta1 <- theta1[ind]
    T <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)

    amostraMCMC <- mcmc(theta1)
    alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
    betaTeorica <-  dadosResultadoABC$BETA_TEORICA 

    denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
    maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
   densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
  })
  output$Drovandi <- renderPlot({
    modelos <- input$modelos
    modelo <- "Drovandi"
    nVertices <- input$selecionaVertices
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#     write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))
    
    if (nVertices == 50) {
      load("amostraABCDrovandi50.RData")
      amostraABC <- amostraABCDrovandi50
      load("dadosResultadoABCDrovandi50.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi50
    }
    if (nVertices == 100) {
      load("amostraABCDrovandi100.RData")
      amostraABC <- amostraABCDrovandi100
      load("dadosResultadoABCDrovandi100.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi100
    }
    if (nVertices == 200) {
      load("amostraABCDrovandi200.RData")
      amostraABC <- amostraABCDrovandi200
      load("dadosResultadoABCDrovandi200.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi200
    }
    if (nVertices == 500) {
      load("amostraABCDrovandi500.RData")
      amostraABC <- amostraABCDrovandi500
      load("dadosResultadoABCDrovandi500.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi500
    }
    if (nVertices == 750) {
      load("amostraABCDrovandi750.RData")
      amostraABC <- amostraABCDrovandi750
      load("dadosResultadoABCDrovandi750.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi750
    }
    if (nVertices == 1000) {
      load("amostraABCDrovandi1000.RData")
      amostraABC <- amostraABCDrovandi1000
      load("dadosResultadoABCDrovandi1000.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi1000
    }
    if (nVertices == 1250) {
      load("amostraABCDrovandi1250.RData")
      amostraABC <- amostraABCDrovandi1250
      load("dadosResultadoABCDrovandi1250.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi1250
    }
    if (nVertices == 1500) {
      load("amostraABCDrovandi1500.RData")
      amostraABC <- amostraABCDrovandi1500
      load("dadosResultadoABCDrovandi1500.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi1500
    }
    if (nVertices == 1750) {
      load("amostraABCDrovandi1750.RData")
      amostraABC <- amostraABCDrovandi1750
      load("dadosResultadoABCDrovandi1750.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi1750
    }
    if (nVertices == 2000) {
      load("amostraABCDrovandi2000.RData")
      amostraABC <- amostraABCDrovandi2000
      load("dadosResultadoABCDrovandi2000.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi2000
    }
    if (nVertices == 2250) {
      load("amostraABCDrovandi2250.RData")
      amostraABC <- amostraABCDrovandi2250
      load("dadosResultadoABCDrovandi2250.RData")
      dadosResultadoABC <- dadosResultadoABCDrovandi2250
    }
    par(mfrow=c(1,4))
    pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
    pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

    lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
    lag <- lagAlgoritmos$LAG
    T <- NROW(amostraABC)
    theta1 <- amostraABC$VL_AMOSTRA_THETA
    ind <- seq(lag,T,by=lag)
    theta1 <- theta1[ind]
    T <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)

    amostraMCMC <- mcmc(theta1)
    alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
    betaTeorica <-  dadosResultadoABC$BETA_TEORICA 

    denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
    maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))

    densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
  })
  output$Rejeicao <- renderPlot({
    modelos <- input$modelos
    modelo <- "Rejeicao"
    nVertices <- input$selecionaVertices
    nV <- as.numeric(nVertices)
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     if (NROW(dadosResultadoABC) != 0) {
    if (nV < 1500) {  
#       amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#       write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))
      
      if (nVertices == 50) {
        load("amostraABCRejeicao50.RData")
        amostraABC <- amostraABCRejeicao50
        load("dadosResultadoABCRejeicao50.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao50
      }
      if (nVertices == 100) {
        load("amostraABCRejeicao100.RData")
        amostraABC <- amostraABCRejeicao100
        load("dadosResultadoABCRejeicao100.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao100
      }
      if (nVertices == 200) {
        load("amostraABCRejeicao200.RData")
        amostraABC <- amostraABCRejeicao200
        load("dadosResultadoABCRejeicao200.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao200
      }
      if (nVertices == 500) {
        load("amostraABCRejeicao500.RData")
        amostraABC <- amostraABCRejeicao500
        load("dadosResultadoABCRejeicao500.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao500
      }
      if (nVertices == 750) {
        load("amostraABCRejeicao750.RData")
        amostraABC <- amostraABCRejeicao750
        load("dadosResultadoABCRejeicao750.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao750
      }
      if (nVertices == 1000) {
        load("amostraABCRejeicao1000.RData")
        amostraABC <- amostraABCRejeicao1000
        load("dadosResultadoABCRejeicao1000.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao1000
      }
      if (nVertices == 1250) {
        load("amostraABCRejeicao1250.RData")
        amostraABC <- amostraABCRejeicao1250
        load("dadosResultadoABCRejeicao1250.RData")
        dadosResultadoABC <- dadosResultadoABCRejeicao1250
      }
      par(mfrow=c(1,4))
      pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
      pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

      lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
      lag <- lagAlgoritmos$LAG
      T <- NROW(amostraABC)
      theta1 <- amostraABC$VL_AMOSTRA_THETA
      ind <- seq(lag,T,by=lag)
      theta1 <- theta1[ind]
      T <- length(ind)
      estTheta1 =cumsum(theta1)/(1:T)
      esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)

      amostraMCMC <- mcmc(theta1)
      alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
      betaTeorica <-  dadosResultadoABC$BETA_TEORICA 

      denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
      maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
      densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
    } else {
      textplot(paste("NÃƒO FORAM GERADOS DADOS PARA ESSE NÃšMERO DE VÃ‰RTICES :",nVertices),cex=1,col.main = "blue",halign = "left",valign="top")
    }  
  })
  output$Marjoram <- renderPlot({
    modelos <- input$modelos
    modelo <- "Marjoram"
    nVertices <- input$selecionaVertices
    nV <- as.numeric(nVertices)
#     dadosResultadoABC  <- subset(dadosResultado, NO_MODELO ==modelo & NU_VERTICES == nVertices)
#     dadosResultadoABC  <- subset(dadosResultadoABC, DT_PROCESSAMENTO == max(DT_PROCESSAMENTO))
#     if (NROW(dadosResultadoABC) != 0) {  
    if (nV < 1250) {  
#       amostraABC <- RetornaAmostraAlgoritmos(conexao, modelo, dadosResultadoABC$DT_PROCESSAMENTO)
#       write.csv(amostraABC,paste("amostraABC_",nVertices,"_",modelo,".csv",sep=""))

      if (nVertices == 50) {
        load("amostraABCMarjoram50.RData")
        amostraABC <- amostraABCMarjoram50
        load("dadosResultadoABCMarjoram50.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram50
      }
      if (nVertices == 100) {
        load("amostraABCMarjoram100.RData")
        amostraABC <- amostraABCMarjoram100
        load("dadosResultadoABCMarjoram100.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram100
      }
      if (nVertices == 200) {
        load("amostraABCMarjoram200.RData")
        amostraABC <- amostraABCMarjoram200
        load("dadosResultadoABCMarjoram200.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram200
      }
      if (nVertices == 500) {
        load("amostraABCMarjoram500.RData")
        amostraABC <- amostraABCMarjoram500
        load("dadosResultadoABCMarjoram500.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram500
      }
      if (nVertices == 750) {
        load("amostraABCMarjoram750.RData")
        amostraABC <- amostraABCMarjoram750
        load("dadosResultadoABCMarjoram750.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram750
      }
      if (nVertices == 1000) {
        load("amostraABCMarjoram1000.RData")
        amostraABC <- amostraABCMarjoram1000
        load("dadosResultadoABCMarjoram1000.RData")
        dadosResultadoABC <- dadosResultadoABCMarjoram1000
      }

      par(mfrow=c(1,4))
      pminXParam <- min(amostraABC$VL_AMOSTRA_THETA)
      pmaxXParam <- max(amostraABC$VL_AMOSTRA_THETA)

      lagAlgoritmos <- subset(lagAlgoritmos, NO_MODELO ==modelo & NU_VERTICES == nVertices)
      lag <- lagAlgoritmos$LAG
      T <- NROW(amostraABC)
      theta1 <- amostraABC$VL_AMOSTRA_THETA
      ind <- seq(lag,T,by=lag)
      theta1 <- theta1[ind]
      T <- length(ind)
      estTheta1 =cumsum(theta1)/(1:T)
      esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T)
      
      amostraMCMC <- mcmc(theta1)
      alphaTeorica <- dadosResultadoABC$ALPHA_TEORICA
      betaTeorica <-  dadosResultadoABC$BETA_TEORICA 

      denAmostraTheta <- density(amostraABC$VL_AMOSTRA_THETA)
      maxYParam <- ifelse(dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica) > max(denAmostraTheta$y), 
                            dbeta(alphaTeorica/(alphaTeorica+betaTeorica),alphaTeorica,betaTeorica), max(denAmostraTheta$y))
    
      densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Probabilidade",
                                      ylab="T=Azul e A=Preta",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste(modelo, 
                                      "\n MÃ©dia a posteriori = ", round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3),
                                      "\n MÃ©dia TeÃ³rica : ",round(as.numeric(dadosResultadoABC$VL_MEDIA_TEORICA),3),
                                      ", Tempo = ", dadosResultadoABC$NU_SEG_PROCESSAMENTO," s",sep="")
                                      )
      pmaxLag <- 100
      curve(dbeta(x,alphaTeorica,betaTeorica),from=pminXParam,to=pmaxXParam,add=TRUE,lwd=2,col="red")
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("KL  : ", round(dadosResultadoABC$KL_TEST_EST,6),"\n KS : ", round(dadosResultadoABC$KS_TEST_EST,4),", p-value :",round(dadosResultadoABC$KS_TEST_PVALUE,6),"\n  Chi-Q : ", round(dadosResultadoABC$CHISQ_TEST_EST,4), sep=""))
#      autocorr.plot(amostraMCMC, lag.max= pmaxLag, auto.layout = FALSE, ask = FALSE, main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))
      mediaAPosteriori <- round(as.numeric(dadosResultadoABC$VL_MEDIA_POSTERIORI),3)
      #abline(h=mediaAPosteriori,col="red")
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("AutocorrelaÃ§Ã£o. \n Taxa de AceitaÃ§Ã£o : ",round(dadosResultadoABC$TX_ACEITACAO,4), "\n Tamanho Efetivo : ",dadosResultadoABC$NEFF,sep=""))

      plot(estTheta1, xlab="IteraÃ§Ãµes",main="MÃ©dia e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T],esterrTheta1[T]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="gold",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="gold",lwd=2)      
      #abline(h=mediaAPosteriori,col="red")
    } else {
      textplot(paste("NÃƒO FORAM GERADOS DADOS PARA ESSE NÃšMERO DE VÃ‰RTICES :",nVertices),cex=1,col.main = "blue",halign = "left",valign="top")
    }  
  })

  output$GraficoTempoEspecifico <- renderPlot({
    modelo <- input$modelo
    nVertices <- input$selecionaVertices

    dadosResultadoTempo <- dadosResultado[c(3,8,9)]
    attach(dadosResultadoTempo)
    dadosResultadoTempo <- dadosResultadoTempo[order(NO_MODELO, NU_VERTICES),]
    dadosResultadoTempo <- subset(dadosResultadoTempo, (NO_MODELO == modelo | NO_MODELO =="Bergm") 
#                                                         & 
#                                                        (NU_VERTICES == 50 | 
#                                                         NU_VERTICES == 100 | 
#                                                         NU_VERTICES == 200 |
#                                                         NU_VERTICES == 500 |
#                                                         NU_VERTICES == 750 |
#                                                         NU_VERTICES == 1000 )
                                  )

    par(mfrow=c(1,1))
    
    colours <- c(Bergm = "black", Delmoral = "blue", Beaumont = "chartreuse", Lenormand = "gold", 
                 Drovandi = "red", Marjoram = "darkmagenta", Rejeicao = "cyan")
    ggplot(dadosResultadoTempo, aes(x = NU_VERTICES, y = NU_SEG_PROCESSAMENTO, colour = NO_MODELO)) + 
      geom_line(size=1) + 
      ylab(label="Tempo em segs") + 
      xlab("VÃ©rtices") + 
      theme_bw() +
      scale_colour_manual(values=colours)  +
      ggtitle(paste("Tempos de Processamento Bergm X ",modelo,".",sep=""))
      

  })

  output$GraficoTempoGeral <- renderPlot({
    modelo <- input$modelo
    nVertices <- input$selecionaVertices
    dadosResultadoTempo <- dadosResultado[c(3,8,9)]
#     dadosResultadoTempo <- subset(dadosResultadoTempo,  NU_VERTICES == 50 | 
#                                                         NU_VERTICES == 100 | 
#                                                         NU_VERTICES == 200 |
#                                                         NU_VERTICES == 500 |
#                                                         NU_VERTICES == 750 |
#                                                         NU_VERTICES == 1000 )
    attach(dadosResultadoTempo)
    dadosResultadoTempo <- dadosResultadoTempo[order(NO_MODELO, NU_VERTICES),]
    
    par(mfrow=c(1,1))

    ggplot(dadosResultadoTempo, aes(x = NU_VERTICES, y = NU_SEG_PROCESSAMENTO, colour = NO_MODELO)) + 
      geom_line(size=1) + 
      ylab(label="Tempo em segs") + 
      xlab("VÃ©rtices") + 
      theme_bw() +
      scale_colour_manual(values=c("chartreuse", "black","blue","red","gold","darkmagenta","cyan")) +
      ggtitle(paste("Tempos de Processamento com TODOS os modelos ABC."))
      
  })

  output$GraficoTempoGeralSemRejeicao <- renderPlot({
    modelo <- input$modelo
    nVertices <- input$selecionaVertices
    dadosResultadoTempo <- dadosResultado[c(3,8,9)]
    dadosResultadoTempo <- subset(dadosResultadoTempo,  NO_MODELO != "Rejeicao" ) 
#     dadosResultadoTempo <- subset(dadosResultadoTempo,  NU_VERTICES == 50 | 
#                                                         NU_VERTICES == 100 | 
#                                                         NU_VERTICES == 200 |
#                                                         NU_VERTICES == 500 |
#                                                         NU_VERTICES == 750 |
#                                                         NU_VERTICES == 1000 )
    attach(dadosResultadoTempo)
    dadosResultadoTempo <- dadosResultadoTempo[order(NO_MODELO, NU_VERTICES),]
    
    par(mfrow=c(1,1))

    ggplot(dadosResultadoTempo, aes(x = NU_VERTICES, y = NU_SEG_PROCESSAMENTO, colour = NO_MODELO)) + 
      geom_line(size=1) + 
      ylab(label="Tempo em segs") + 
      xlab("VÃ©rtices") + 
      theme_bw() +
      scale_colour_manual(values=c("chartreuse", "black","blue","red","gold","darkmagenta","cyan")) +
      ggtitle(paste("Tempos de Processamento Sem ABC - Rejeicao."))
      
  })
      # Generate a summary of the data
  output$summary <- renderPrint({
    summary(dados)
  })
  
  # Generate an HTML table view of the data
  output$estatistica50 <- renderDataTable(
      dadosResultado50,
    options = list(
      rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                 ), 
                     searching = FALSE,
                     paging = FALSE)
  )

  output$estatistica100 <- renderDataTable(
      dadosResultado100,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
         ), searching = FALSE,
                     paging = FALSE)
  )

  output$estatistica200 <- renderDataTable(
      dadosResultado200,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica500 <- renderDataTable(
      dadosResultado500,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica750 <- renderDataTable(
      dadosResultado750,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )

  output$estatistica1000 <- renderDataTable(
      dadosResultado1000,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        } 
      // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1250 <- renderDataTable(
      dadosResultado1250,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        } 
      // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1500 <- renderDataTable(
      dadosResultado1500,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        }
      // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1750 <- renderDataTable(
      dadosResultado1750,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        }
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica2000 <- renderDataTable(
      dadosResultado2000,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        }
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica2250 <- renderDataTable(
      dadosResultado2250,
    options = list(rowCallback = I(
      'function(row, data) {
        if (parseFloat(data[2]) <= parseFloat(0.70)) {
            $("td:eq(2)", row).css("font-weight", "bold");
            $("td:eq(2)", row).css("color", "red");
        }
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[7])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[9])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[11])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[12])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "black");
        $("th:eq(7)").css("color", "black");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "black");
        $("th:eq(8)").css("color", "black");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "black");
        $("th:eq(9)").css("color", "black");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "black");
        $("th:eq(10)").css("color", "black");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "black");
        $("th:eq(11)").css("color", "black");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "black");
        $("th:eq(12)").css("color", "black");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
})