require(dplyr)
require(tidyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

calcular_notas_numero_acertos <- function(pathMicrodados) {
  accepted_columns <- c(
    "CO_PROVA_CN","CO_PROVA_CH","CO_PROVA_LC","CO_PROVA_MT",
    "NU_NOTA_CN","NU_NOTA_CH","NU_NOTA_LC","NU_NOTA_MT",
    "TX_RESPOSTAS_CN","TX_RESPOSTAS_CH","TX_RESPOSTAS_LC","TX_RESPOSTAS_MT",
    "TX_GABARITO_CN","TX_GABARITO_CH","TX_GABARITO_LC","TX_GABARITO_MT"
  )
  
  chunkSize <- 30
  microdados <- read.csv(pathMicrodados, nrows=chunkSize,  fill=TRUE, na.strings = T, sep=",")
  microdados <- microdados[accepted_columns]
  
  microdados$ACERTOS_CH <- rep(0, nrow(microdados))
  microdados$ACERTOS_LC <- rep(0, nrow(microdados))
  microdados$ACERTOS_MT <- rep(0, nrow(microdados))
  microdados$ACERTOS_CN <- rep(0, nrow(microdados))
  
  for(i in 1:nrow(microdados)) {
    candidato <- microdados[i,]
    for(tipo_prova in c("CH", "LC", "MT", "CN")) {
      aux <- paste("TX_GABARITO_", tipo_prova ,sep='')
      gabarito <- unlist(strsplit(candidato[,aux], ''))
      aux <- paste("TX_RESPOSTAS_", tipo_prova ,sep='')
      respostas <- unlist(strsplit(candidato[,aux], ''))
      aux <- paste("ACERTOS_", tipo_prova ,sep='')
      for(j in 1:length(respostas)) {
        if(respostas[j] == gabarito[j]) {
          microdados[i,aux] <- microdados[i,aux] + 1
        }
      }
    }
  }
  
  microdados_out <- microdados %>% 
    select(
      "NU_NOTA_CN","NU_NOTA_CH",
      "NU_NOTA_LC","NU_NOTA_MT",
      "ACERTOS_CN","ACERTOS_CH",
      "ACERTOS_LC","ACERTOS_MT",
    )
  
  return(microdados_out)
}

###############

files <- data.frame(
  "MICRODADOS_ENEM_2016_PE.csv",
  "MICRODADOS_ENEM_2017_PE.csv",
  "MICRODADOS_ENEM_2018_PE.csv",
  "MICRODADOS_ENEM_2019_PE.csv"
)

ano <- 2016
for(i in c(1:4)) {
  pathDir <- "./datasets_2016_2019_pernambuco/"
  pathMicrodados <- paste(pathDir, files[i], sep = '')
  microdados_out <- calcular_notas_numero_acertos(pathMicrodados)
  output_file <- paste(
    "./datasets_2016_2019_pernambuco/",
    "NOTAS_E_NUMERO_DE_ACERTOS_",
    ano,".csv", sep = '')
  write.csv(microdados_out, output_file)
  ano <- ano + 1
}
