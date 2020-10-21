require(dplyr)
require(tidyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

microdados_path <- "./microdados/MICRODADOS_ENEM_2016.csv"

accepted_columns <- c(
  "TP_PRESENCA_CN","TP_PRESENCA_CH","TP_PRESENCA_LC","TP_PRESENCA_MT",
  "CO_PROVA_CN","CO_PROVA_CH","CO_PROVA_LC","CO_PROVA_MT",
  "NU_NOTA_CN","NU_NOTA_CH","NU_NOTA_LC","NU_NOTA_MT",
  "TX_RESPOSTAS_CN","TX_RESPOSTAS_CH","TX_RESPOSTAS_LC","TX_RESPOSTAS_MT",
  "TX_GABARITO_CN","TX_GABARITO_CH","TX_GABARITO_LC","TX_GABARITO_MT"
)

chunkSize <- 300
microdados <- read.csv(microdados_path, nrows=chunkSize,  fill=TRUE, na.strings = T, sep=";")
microdados <- microdados[accepted_columns]

microdados$ACERTOS_CH <- rep(0, nrow(microdados))
microdados$ACERTOS_LC <- rep(0, nrow(microdados))
microdados$ACERTOS_MT <- rep(0, nrow(microdados))
microdados$ACERTOS_CN <- rep(0, nrow(microdados))

for(i in 1:nrow(microdados)) {
  candidato <- microdados[i,]
  for(tipo_prova in c("CH", "LC", "MT", "CN")) {
    aux <- paste("TP_PRESENCA_", tipo_prova ,sep='')
    if(candidato[,aux] == 1) {
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
}

microdados_out <- microdados %>% 
  filter(
      TP_PRESENCA_CH==1 & 
      TP_PRESENCA_LC==1 & 
      TP_PRESENCA_MT==1 & 
      TP_PRESENCA_CN==1 ) %>% 
  select(
    "NU_NOTA_CN","NU_NOTA_CH",
    "NU_NOTA_LC","NU_NOTA_MT",
    "ACERTOS_CN","ACERTOS_CH",
    "ACERTOS_LC","ACERTOS_MT") %>% 
  slice(c(1:100))

write.csv(microdados_out,'Notas_e_Numero_de_Acertos_2016.csv')
