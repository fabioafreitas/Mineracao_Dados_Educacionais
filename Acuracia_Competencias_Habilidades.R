#install.packages("dplyr")
#install.packages("tidyr")

require(dplyr)
require(tidyr)
require(readr)

calular_acuracia <- function(lista_candidados, itens_prova) {
  habilidades <- data.frame(
    "nome_municipio"=rep("",30),
    "num_habilidade"=c(1:30),
    "ch.total"=rep(0, 30),
    "lc.total"=rep(0, 30),
    "mt.total"=rep(0, 30),
    "cn.total"=rep(0, 30),
    "ch.acertos"=rep(0, 30),
    "lc.acertos"=rep(0, 30),
    "mt.acertos"=rep(0, 30),
    "cn.acertos"=rep(0, 30)
  )
  for(i in 1:nrow(lista_candidados)) {
    candidato <- lista_candidados[i,]
    for(tipo_prova in c("CH","CN","MT","LC")) {
      aux <- paste("TX_GABARITO_",tipo_prova,sep = '')
      gabarito <- unlist(strsplit(paste(candidato[,aux]), ''))
      aux <- paste("TX_RESPOSTAS_",tipo_prova,sep = '')
      respostas <- unlist(strsplit(paste(candidato[,aux]), ''))
      aux <- paste("CO_PROVA_",tipo_prova,sep = '')
      codigo_prova <- as.numeric(candidato[,aux])
      for(j in 1:length(respostas)) {
        habilidade <- itens_prova %>% filter(CO_PROVA == codigo_prova & CO_POSICAO == j)
        index <- habilidade$CO_HABILIDADE
        aux <- paste(tolower(tipo_prova), ".total",sep = '')
        habilidades[,aux][index] <- habilidades[,aux][index] + 1
        if(respostas[j] == gabarito[j]) {
          aux <- paste(tolower(tipo_prova), ".acertos",sep = '')
          habilidades[,aux][index] <- habilidades[,aux][index] + 1
        }
      }
    }
  }
  return(habilidades)
}


calcular_acuracia_municipios <- function(pathMicrodados, pathItensProva) {
  cols_to_read = c(
    "NO_MUNICIPIO_RESIDENCIA",
    "CO_PROVA_CN","CO_PROVA_CH",
    "CO_PROVA_LC","CO_PROVA_MT",
    "TX_RESPOSTAS_CN","TX_RESPOSTAS_CH",
    "TX_RESPOSTAS_LC","TX_RESPOSTAS_MT",
    "TX_GABARITO_CN","TX_GABARITO_CH",
    "TX_GABARITO_LC","TX_GABARITO_MT"
  )
  
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  itens_prova <- read_csv2(pathItensProva, col_types = cols())
  microdados <- read_csv(pathMicrodados, col_types = cols())
  microdados <- microdados[cols_to_read]
  
  microdados <- microdados[1:50,]
  
  microdados$NO_MUNICIPIO_RESIDENCIA <- as.factor(microdados$NO_MUNICIPIO_RESIDENCIA)
  acuracia_municipios <- data.frame()

  primeiraIteracao <- TRUE
  for(municipio_atual in levels(microdados$NO_MUNICIPIO_RESIDENCIA)) {
    candidatos <- microdados %>% filter(NO_MUNICIPIO_RESIDENCIA == municipio_atual)
    acuracia_municipio_atual <- calular_acuracia(candidatos, itens_prova)
    acuracia_municipio_atual$nome_municipio <- municipio_atual
    acuracia_municipio_atual <- acuracia_municipio_atual %>% mutate(ch.acuracia=ch.acertos/ch.total)
    acuracia_municipio_atual <- acuracia_municipio_atual %>% mutate(lc.acuracia=lc.acertos/lc.total)
    acuracia_municipio_atual <- acuracia_municipio_atual %>% mutate(mt.acuracia=mt.acertos/mt.total)
    acuracia_municipio_atual <- acuracia_municipio_atual %>% mutate(cn.acuracia=cn.acertos/cn.total)
    if(primeiraIteracao) {
      primeiraIteracao <- FALSE
      acuracia_municipios <- acuracia_municipio_atual
    } else {
      acuracia_municipios <- rbind(acuracia_municipios, acuracia_municipio_atual)
    }
  }
  
  return(acuracia_municipios)
}

files <- data.frame(
  microdados=c(
    "MICRODADOS_ENEM_2016_PE.csv",
    "MICRODADOS_ENEM_2017_PE.csv",
    "MICRODADOS_ENEM_2018_PE.csv",
    "MICRODADOS_ENEM_2019_PE.csv"
  ),
  itens_prova=c(
    "ITENS_PROVA_2016.csv",
    "ITENS_PROVA_2017.csv",
    "ITENS_PROVA_2018.csv",
    "ITENS_PROVA_2019.csv"
  )
)

ano <- 2016
for(i in c(1:4)) {
  pathDir <- "./datasets_2016_2019_pernambuco/"
  pathItensProva <- paste(pathDir, files$itens_prova[i], sep = '')
  pathMicrodados <- paste(pathDir, files$microdados[i], sep = '')
  acuracia_municipios <- calcular_acuracia_municipios(pathMicrodados, pathItensProva)
  output_file <- paste(
    "./datasets_2016_2019_pernambuco/",
    "ACURACIA_COMPETENCIAS_HABILIDADES_", 
    ano,".csv", sep = '')
  write.csv(acuracia_municipios, output_file)
  ano <- ano + 1
}


