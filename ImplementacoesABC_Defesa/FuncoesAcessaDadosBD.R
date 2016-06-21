

RetornaConexaoBDOracle <- function (pConfig) {
  stringConexao <- pConfig$oracle$stringConexao
  arqJar        <- pConfig$oracle$arqJar
  drv <- JDBC("oracle.jdbc.driver.OracleDriver", arqJar, "`")
  conn <- dbConnect(drv, stringConexao)
}

FechaConexaoBDOracle <- function (pConexao) {
  dbDisconnect(pConexao)
}

RetornaResultadoAlgoritmos <- function (pConexao) {
  resultadoAlgoritmos = tryCatch({
    resultadoAlgoritmos <- dbGetQuery(pConexao, paste("SELECT * FROM DBDMINFOSAS.TB_DABC_RESULTADO_Q"))
    return(resultadoAlgoritmos)
  }, warning = function(w) {
    print(w)
    gWMsg <<- paste("Warning Acesso Dados Resultado : ",w)
    return(gWMsg)
  }, error = function(e) {
    print(e)
    gEMsg <<- paste("Erro Acesso Dados Resultado : ",e)
    return(gEMsg)
  }, finally = {
  })
  return(resultadoAlgoritmos)
}

RetornaLagAlgoritmos <- function (pConexao) {
  lagAlgoritmos = tryCatch({
    lagAlgoritmos <- dbGetQuery(pConexao, paste("SELECT * FROM DBDMINFOSAS.TB_DABC_LAG"))
    return(lagAlgoritmos)
  }, warning = function(w) {
    print(w)
    gWMsg <<- paste("Warning Acesso Dados Lag : ",w)
    return(gWMsg)
  }, error = function(e) {
    print(e)
    gEMsg <<- paste("Erro Acesso Dados Lag : ",e)
    return(gEMsg)
  }, finally = {
  })
  return(lagAlgoritmos)
}

RetornaAmostraAlgoritmos <- function (pConexao, pModelo, pData) {
  amostraAlgoritmos = tryCatch({
    amostraAlgoritmos <- dbGetQuery(pConexao, paste("SELECT * 
                                                         FROM DBDMINFOSAS.TB_DABC_AMOSTRA_POSTERIORI 
                                                        WHERE DT_PROCESSAMENTO = '",pData,
                                                        "' AND NO_MODELO = '",pModelo,"'",sep=""))
    return(amostraAlgoritmos)
  }, warning = function(w) {
    print(w)
    gWMsg <<- paste("Warning Acesso Dados de Amostra : ",w)
    return(gWMsg)
  }, error = function(e) {
    print(e)
    gEMsg <<- paste("Erro Acesso Dados de Amostra : ",e)
    return(gEMsg)
  }, finally = {
  })
  return(amostraAlgoritmos)
}

GravaDadosBD <- function (pConexao, pTabela, ...) {
    resultBD <- tryCatch({
      if (pTabela == "TB_DABC_RESULTADO") {
        comandoInsert <- paste ('INSERT INTO DBDMINFOSAS.TB_DABC_RESULTADO (DT_PROCESSAMENTO,NO_ALGORITMO, NO_MODELO, VL_THETA1, VL_PROBABILIDADE, VL_MEDIA_POSTERIORI , 
                                 VL_MEDIA_TEORICA,NU_VERTICES, NU_SEG_PROCESSAMENTO, TX_ACEITACAO , KL_TEST_EST, KS_TEST_EST, KS_TEST_PVALUE, CHISQ_TEST_EST,NEFF,CV,ALPHA_TEORICA,BETA_TEORICA,TIPO_PROC  ) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)')   
        resultBD <- dbSendUpdate (pConexao, comandoInsert,DT_PROCESSAMENTO,
                                                        NO_ALGORITMO  						,
              																					NO_MODELO     						,
              																					VL_THETA1     						,
              																					VL_PROBABILIDADE       		,           
              																					VL_MEDIA_POSTERIORI       ,           
              																					VL_MEDIA_TEORICA          ,            
              																					NU_VERTICES               ,
              																					NU_SEG_PROCESSAMENTO      ,
              																					TX_ACEITACAO              ,
              																					KL_TEST_EST               ,
              																					KS_TEST_EST               ,
              																					KS_TEST_PVALUE            ,
              																					CHISQ_TEST_EST,
                                                        NEFF,
                                                        CV,
                                                        ALPHA_TEORICA,
                                                        BETA_TEORICA,
                                                        TIPO_PROC)
        resultBD <- 0
        return(resultBD)
      }
  
    }, warning = function(w) {
      print(w)
      gWMsg <<- paste("Warning Insert TB_DABC_RESULTADO : ",w)
      return(gWMsg)
    }, error = function(e) {
      print(e)
      gEMsg <<- paste("Erro Insert TB_DABC_RESULTADO : ",e)
      return(gEMsg)
    }, finally = {
    })
    return(resultBD)
}
