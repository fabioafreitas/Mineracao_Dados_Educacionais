#install.packages("dplyr")
#install.packages("tidyr")

require(dplyr)
require(tidyr)
require(readr)

?read_csv
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##### COLUNAS #####
cols_to_read = c(
  "NO_MUNICIPIO_RESIDENCIA",
  "CO_PROVA_CN",
  "CO_PROVA_CH",
  "CO_PROVA_LC",
  "CO_PROVA_MT",
  "TX_RESPOSTAS_CN",
  "TX_RESPOSTAS_CH",
  "TX_RESPOSTAS_LC",
  "TX_RESPOSTAS_MT",
  "TX_GABARITO_CN",
  "TX_GABARITO_CH",
  "TX_GABARITO_LC",
  "TX_GABARITO_MT"
)

##### LEITURA #####
?read_delim
microdados_file <- "./datasets_2016_2019_pernambuco/MICRODADOS_ENEM_2016_PE.csv"
input_data_file <- "./datasets_2016_2019_pernambuco/ITENS_PROVA_2016.csv"
microdados <- read_csv(microdados_file, col_types = cols())
itens_prova <- read_csv2(input_data_file, col_types = cols())
microdados <- microdados[cols_to_read]

#pegando os candidados por municipio
municipio <- "Recife" 
candidatos_cidade <- microdados %>% filter(NO_MUNICIPIO_RESIDENCIA == municipio)

##### CALCULOS RELACIONADOS A HABILIDADES POR MUNICIPIO #####


calular_acuracia <- function(lista_candidados) {
  habilidades <- data.frame(
    "ch.total"=rep(0, 30),
    "ch.acertos"=rep(0, 30),
    "lc.total"=rep(0, 30),
    "lc.acertos"=rep(0, 30),
    "mt.total"=rep(0, 30),
    "mt.acertos"=rep(0, 30),
    "cn.total"=rep(0, 30),
    "cn.acertos"=rep(0, 30)
  )
  
  
  for(i in 1:nrow(candidatos_cidade)) {
    candidato <- candidatos_cidade[i,]
    
    gabarito <- unlist(strsplit(candidato$TX_GABARITO_CH, ''))
    respostas <- unlist(strsplit(candidato$TX_RESPOSTAS_CH, ''))
    codigo_prova <- candidato$CO_PROVA_CH
    
    for(j in 1:length(respostas)) {
      habilidade <- itens_prova %>% filter(CO_PROVA == codigo_prova & CO_POSICAO == j)
      index <- habilidade$CO_HABILIDADE
      habilidades$ch.total[index] <- habilidades$ch.total[index] + 1
      if(respostas[j] == gabarito[j]) {
        habilidades$ch.acertos[index] <- habilidades$ch.acertos[index] + 1
      }
    }
    
    gabarito <- unlist(strsplit(candidato$TX_GABARITO_LC, ''))
    respostas <- unlist(strsplit(candidato$TX_RESPOSTAS_LC, ''))
    codigo_prova <- candidato$CO_PROVA_LC
    
    for(j in 1:length(respostas)) {
      habilidade <- itens_prova %>% filter(CO_PROVA == codigo_prova & CO_POSICAO == j)
      index <- habilidade$CO_HABILIDADE
      habilidades$lc.total[index] <- habilidades$lc.total[index] + 1
      if(respostas[j] == gabarito[j]) {
        habilidades$lc.acertos[index] <- habilidades$lc.acertos[index] + 1
      }
    }
    
    gabarito <- unlist(strsplit(candidato$TX_GABARITO_MT, ''))
    respostas <- unlist(strsplit(candidato$TX_RESPOSTAS_MT, ''))
    codigo_prova <- candidato$CO_PROVA_MT
    
    for(j in 1:length(respostas)) {
      habilidade <- itens_prova %>% filter(CO_PROVA == codigo_prova & CO_POSICAO == j)
      index <- habilidade$CO_HABILIDADE
      habilidades$mt.total[index] <- habilidades$mt.total[index] + 1
      if(respostas[j] == gabarito[j]) {
        habilidades$mt.acertos[index] <- habilidades$mt.acertos[index] + 1
      }
    }
    
    gabarito <- unlist(strsplit(candidato$TX_GABARITO_CN, ''))
    respostas <- unlist(strsplit(candidato$TX_RESPOSTAS_CN, ''))
    codigo_prova <- candidato$CO_PROVA_CN
    
    for(j in 1:length(respostas)) {
      habilidade <- itens_prova %>% filter(CO_PROVA == codigo_prova & CO_POSICAO == j)
      index <- habilidade$CO_HABILIDADE
      habilidades$cn.total[index] <- habilidades$cn.total[index] + 1
      if(respostas[j] == gabarito[j]) {
        habilidades$cn.acertos[index] <- habilidades$cn.acertos[index] + 1
      }
    }
  }
  
  return(habilidades)
}

##### CALCULANDO ACURACIA POR HABILIDADE ####

candidatos_cidade <- candidatos_cidade[1:10,]

habilidades <- calular_acuracia(candidatos_cidade)

habilidades <- habilidades %>% mutate(ch.acuracia=ch.acertos/ch.total)
habilidades <- habilidades %>% mutate(lc.acuracia=lc.acertos/lc.total)
habilidades <- habilidades %>% mutate(mt.acuracia=mt.acertos/mt.total)
habilidades <- habilidades %>% mutate(cn.acuracia=cn.acertos/cn.total)

habilidades %>% select(ch.acuracia, lc.acuracia, mt.acuracia, cn.acuracia)
