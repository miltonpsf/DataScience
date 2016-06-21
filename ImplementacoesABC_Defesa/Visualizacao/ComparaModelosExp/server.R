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
myDir<-"C:/MiltonNotebook/Mestrado Estatistica/2014/9-Dissertacao/ImplementacoesABC_Defesa"
configFile <- paste(myDir, "config.yml", sep="/");
config <- yaml.load_file(configFile)
setwd(config$diretorios$DadosDir)

fu <- function (u, theta1, theta2) theta1*u + theta2*u^3 -u/2*log(u) - (1 - u )/2*log(1 - u)

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
  intervaloConfPValuePorV <- as.data.frame(t(c(0.05,0.1)))
  names(intervaloConfPValuePorV) <- c("PV95","PV99")
#  dadosResultadoPorV <- cbind(dadosResultadoPorV,intervaloConfKLPorV,intervaloConfKSPorV,intervaloConfChiPorV,intervaloConfPValuePorV)
  dadosResultadoPorV <- cbind(dadosResultadoPorV,intervaloConfKLPorV,intervaloConfKSPorV,intervaloConfPValuePorV)
  attach(dadosResultadoPorV)
  dadosResultadoPorV <- dadosResultadoPorV[order(Modelo),]
  return(dadosResultadoPorV)
}
#dadosResultado50   <- retornaDadosResutadoPorV(dadosResultado,50,intervaloConfKL, intervaloConfKS, intervaloConfChi2)

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
    
    nCenario  <- switch(input$selecionaCenario)

  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$BergmG <- renderPlot({
    modelo <- input$modelo
    nVertices <- input$selecionaVertices
    nCenario <- input$selecionaCenario
    if (nCenario == 802 || nCenario == 902) {
      if (nCenario == 802) {
        load("amostraTheta1_Bergm50_8_02.RData")
        load("amostraTheta2_Bergm50_8_02.RData")
        amostra1Bergm <- amostraTheta1
        amostra2Bergm <- amostraTheta2
        thetasG <- c(0.8,0.02)
      }
      if (nCenario == 902) {
        load("amostraTheta1_Bergm50_9_02.RData")
        load("amostraTheta2_Bergm50_9_02.RData")
        amostra1Bergm <- amostraTheta1
        amostra2Bergm <- amostraTheta2
        thetasG <- c(0.9,0.02)
      }
      guEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = thetasG[1], theta2 = thetasG[2] )
      
      par(mfrow=c(2,4))
      pminXParam <- min(amostra1Bergm)
      pmaxXParam <- max(amostra1Bergm)
      
      lag <- 1
      T1 <- NROW(amostra1Bergm)
      theta1 <- amostra1Bergm
      ind <- seq(lag,T1,by=lag)
      theta1 <- theta1[ind]
      T1 <- length(ind)
      estTheta1 =cumsum(theta1)/(1:T1)
      esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T1)
      
      T2 <- NROW(amostra2Bergm)
      theta2 <- amostra2Bergm
      ind <- seq(lag,T2,by=lag)
      theta2 <- theta2[ind]
      T2 <- length(ind)
      estTheta2 =cumsum(theta2)/(1:T2)
      esterrTheta2=sqrt(cumsum((theta2-estTheta2)^2))/(1:T2)
      
      mediaAPosteriori1 <- mean(theta1)
      mediaAPosteriori2 <- mean(theta2)
      guEstrelaHat <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = mediaAPosteriori1, theta2 = mediaAPosteriori2 )
      
      amostraMCMC <- mcmc(theta1)
      
      denAmostraTheta <- density(amostra1Bergm)
      maxYParam <- max(denAmostraTheta$y)
      densplot(amostraMCMC,type="l", lwd=2,
               xlab="Densidade",
               xlim=c(pminXParam,pmaxXParam),
               ylim=c(0,maxYParam),
               main=paste("Theta1=",thetasG[1],"\n p=",round(guEstrela$maximum,3)," pHat=",round(guEstrelaHat$maximum,3),"\n Média posteriori =",round(mediaAPosteriori1,3),sep="")
      )
      
      abline(v=mediaAPosteriori1,col="red")
      pmaxLag <- 100
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("Convergência"))
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("Autocorrelaçãoo",sep=""))
      
      plot(estTheta1, xlab="Iterações",main="Média e intervalo de erro : Theta 1",type="l",lwd=
             + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T1],esterrTheta1[T1]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="blue",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="blue",lwd=2)      
      
      lag <- 1
      pminXParam <- min(amostra2Bergm)
      pmaxXParam <- max(amostra2Bergm)
      
      amostraMCMC <- mcmc(theta2)
      mediaAPosteriori2 <- mean(theta2)
      
      denAmostraTheta <- density(amostra2Bergm)
      maxYParam <- max(denAmostraTheta$y)
      densplot(amostraMCMC,type="l", lwd=2,
               xlab="Densidade",
               xlim=c(pminXParam,pmaxXParam),
               ylim=c(0,maxYParam),
               main=paste("Theta2=",thetasG[2],"\n p=",round(guEstrela$maximum,3)," pHat=",round(guEstrelaHat$maximum,3),"\n Média posteriori =",round(mediaAPosteriori2,3),sep="")
      )
      
      abline(v=mediaAPosteriori2,col="red")
      pmaxLag <- 100
      
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("Convergência"))
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("Autocorrelaçãoo",sep=""))
      
      plot(estTheta2, xlab="Iterações",main="Média e intervalo de erro : Theta 2",type="l",lwd=
             + 2,ylim=mean(theta2)+20*c(-esterrTheta2[T2],esterrTheta2[T2]),ylab="")
      lines(estTheta2 +2*esterrTheta2,col="green",lwd=2)
      lines(estTheta2 -2*esterrTheta2,col="green",lwd=2)      
    } else {
        textplot(paste("NÃO FORAM GERADOS DADOS PARA ESSE CENÁRIO"),cex=1,col.main = "blue",halign = "left",valign="top")
    }
  })

  output$Lenormand <- renderPlot({
    modelos <- input$modelos
    modelo <- "Lenormand"
    nVertices <- input$selecionaVertices
    nCenario <- input$selecionaCenario
    if (nCenario == 802) {
        load("amostraTheta1_Lenorman50_O_8_02.RData")
        load("amostraTheta2_Lenorman50_O_8_02.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.8,0.02)
    }
    if (nCenario == 902) {
        load("amostraTheta1_Lenorman50_O_9_02.RData")
        load("amostraTheta2_Lenorman50_O_9_02.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.9,0.02)
    }
    if (nCenario == 22) {
        load("amostraTheta1_Lenorman50_O_2_2.RData")
        load("amostraTheta2_Lenorman50_O_2_2.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.2,0.2)
    }
    if (nCenario == 535) {
        load("amostraTheta1_Lenorman50_O_5_35.RData")
        load("amostraTheta2_Lenorman50_O_5_35.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.5,0.35)
    }
    if (nCenario == 61) {
        load("amostraTheta1_Lenorman50_O_6_1.RData")
        load("amostraTheta2_Lenorman50_O_6_1.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.6,0.1)
    }
    if (nCenario == 73) {
        load("amostraTheta1_Lenorman50_O_7_3.RData")
        load("amostraTheta2_Lenorman50_O_7_3.RData")
        amostra1OLenormand <- amostraTheta1
        amostra2OLenormand <- amostraTheta2
        thetasG <- c(0.7,0.3)
    }
    guEstrela <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = thetasG[1], theta2 = thetasG[2] )

    par(mfrow=c(2,4))
    pminXParam <- min(amostra1OLenormand)
    pmaxXParam <- max(amostra1OLenormand)

    lag <- 1
    T1 <- NROW(amostra1OLenormand)
    theta1 <- amostra1OLenormand
    ind <- seq(lag,T1,by=lag)
    theta1 <- theta1[ind]
    T1 <- length(ind)
    estTheta1 =cumsum(theta1)/(1:T1)
    esterrTheta1=sqrt(cumsum((theta1-estTheta1)^2))/(1:T1)

    T2 <- NROW(amostra2OLenormand)
    theta2 <- amostra2OLenormand
    ind <- seq(lag,T2,by=lag)
    theta2 <- theta2[ind]
    T2 <- length(ind)
    estTheta2 =cumsum(theta2)/(1:T2)
    esterrTheta2=sqrt(cumsum((theta2-estTheta2)^2))/(1:T2)

    mediaAPosteriori1 <- mean(theta1)
    mediaAPosteriori2 <- mean(theta2)
    guEstrelaHat <- optimize(f = fu, c(0,1), maximum = TRUE, theta1 = mediaAPosteriori1, theta2 = mediaAPosteriori2 )

    amostraMCMC <- mcmc(theta1)
    denAmostraTheta <- density(amostra1OLenormand)
    maxYParam <- max(denAmostraTheta$y)
    
    densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Densidade",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste("Theta1=",thetasG[1],"\n p=",round(guEstrela$maximum,3)," pHat=",round(guEstrelaHat$maximum,3),"\n Média posteriori =",round(mediaAPosteriori1,3),sep="")
                                      )
      abline(v=mediaAPosteriori1,col="red")
      pmaxLag <- 100
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("Convergência"))
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("Autocorrelaçãoo",sep=""))

      plot(estTheta1, xlab="Iterações",main="Média e intervalo de erro : Theta 1",type="l",lwd=
      + 2,ylim=mean(theta1)+20*c(-esterrTheta1[T1],esterrTheta1[T1]),ylab="")
      lines(estTheta1 +2*esterrTheta1,col="blue",lwd=2)
      lines(estTheta1 -2*esterrTheta1,col="blue",lwd=2)      

    pminXParam <- min(amostra2OLenormand)
    pmaxXParam <- max(amostra2OLenormand)

    amostraMCMC <- mcmc(theta2)

    denAmostraTheta <- density(amostra2OLenormand)
    maxYParam <- max(denAmostraTheta$y)
    
    densplot(amostraMCMC,type="l", lwd=2,
                                      xlab="Densidade",
                                      xlim=c(pminXParam,pmaxXParam),
                                      ylim=c(0,maxYParam),
                                      main=paste("Theta2=",thetasG[2],"\n p=",round(guEstrela$maximum,3)," pHat=",round(guEstrelaHat$maximum,3),"\n Média posteriori =",round(mediaAPosteriori2,3),sep="")
                                      )
      abline(v=mediaAPosteriori2,col="red")
      pmaxLag <- 100
      traceplot(amostraMCMC, ylim=c(pminXParam,pmaxXParam), main = paste("Convergência"))
      acf(amostraMCMC,lag.max= pmaxLag, type="correlation", main = paste("Autocorrelaçãoo",sep=""))

      plot(estTheta2, xlab="Iterações",main="Média e intervalo de erro : Theta 2",type="l",lwd=
      + 2,ylim=mean(theta2)+20*c(-esterrTheta2[T2],esterrTheta2[T2]),ylab="")
      lines(estTheta2 +2*esterrTheta2,col="green",lwd=2)
      lines(estTheta2 -2*esterrTheta2,col="green",lwd=2)      
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
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

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
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
         ), searching = FALSE,
                     paging = FALSE)
  )

  output$estatistica200 <- renderDataTable(
      dadosResultado200,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica500 <- renderDataTable(
      dadosResultado500,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica750 <- renderDataTable(
      dadosResultado750,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )

  output$estatistica1000 <- renderDataTable(
      dadosResultado1000,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1250 <- renderDataTable(
      dadosResultado1250,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1500 <- renderDataTable(
      dadosResultado1500,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica1750 <- renderDataTable(
      dadosResultado1750,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica2000 <- renderDataTable(
      dadosResultado2000,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
  output$estatistica2250 <- renderDataTable(
      dadosResultado2250,
    options = list(rowCallback = I(
      'function(row, data) {
        // KL
        if (parseFloat(data[3]) <= parseFloat(data[8])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "black");
        } else 
        if (parseFloat(data[3]) <= parseFloat(data[9])) {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "blue");
        } else {
          $("td:eq(3)", row).css("font-weight", "bold");
          $("td:eq(3)", row).css("color", "red");
        } 
        // KS
        if (parseFloat(data[4]) <= parseFloat(data[10])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "black");
        } else 
        if (parseFloat(data[4]) <= parseFloat(data[11])) {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "blue");
        } else {
          $("td:eq(4)", row).css("font-weight", "bold");
          $("td:eq(4)", row).css("color", "red");
        }
        // p-value
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "black");
        } else 
        if (parseFloat(data[5]) >= parseFloat(data[15])) {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "blue");
        } else {
          $("td:eq(5)", row).css("font-weight", "bold");
          $("td:eq(5)", row).css("color", "red");
        } 
        $("td:eq(6)", row).css("font-weight", "normal");
        $("td:eq(6)", row).css("color", "black");
        $("th:eq(6)").css("color", "black");

        $("td:eq(7)", row).css("font-weight", "normal");
        $("td:eq(7)", row).css("color", "white");
        $("th:eq(7)").css("color", "white");
        
        $("td:eq(8)", row).css("font-weight", "normal");
        $("td:eq(8)", row).css("color", "white");
        $("th:eq(8)").css("color", "white");

        $("td:eq(9)", row).css("font-weight", "normal");
        $("td:eq(9)", row).css("color", "white");
        $("th:eq(9)").css("color", "white");

        $("td:eq(10)", row).css("font-weight", "normal");
        $("td:eq(10)", row).css("color", "white");
        $("th:eq(10)").css("color", "white");

        $("td:eq(11)", row).css("font-weight", "normal");
        $("td:eq(11)", row).css("color", "white");
        $("th:eq(11)").css("color", "white");

        $("td:eq(12)", row).css("font-weight", "normal");
        $("td:eq(12)", row).css("color", "white");
        $("th:eq(12)").css("color", "white");

      }'
                  ), searching = FALSE,
                     paging = FALSE)
  )
  
})