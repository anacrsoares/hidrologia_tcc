## Import Libraries
library(tidyverse)
library(hydrobr)
library(ggplot2)
library(sf)
library(dplyr)

## Read.me hydrobr library (optional)
#vignette(package = "hydrobr", topic = "intro_to_hydrobr")

## Creating stations dictionary
station_codes <- data.frame(x= c("croata","fazenda_boa_esperanca","moraujo", 
                                 "granja", "fazenda_cajazeiras","acude_araras",
                                 "trapia",
                                 "groairas", "fazenda_bela_vista", "sitios_novos",
                                 "umarituba_novo", "barra_nova", "cristais",
                                 "arneiroz", "malhada", "sitio_conceicao",
                                 "sitio_poco_dantas", "carius", "iguatu", "sitio_lapinha",
                                 "podimirim", "lavras_magabeira", "ico", "senador_pompeu",
                                 "quixeramobim", "bau", "aracoiaba", "chorozinho",
                                 "sitio_poco_paus", "sussurana",  "santo_antonio",
                                 "boqueirao_patu", "boq_pedras_brancas"
                                 ),
                            y= c("34730000", "34750000", "35125000", "35170000",
                                 "35210000", "35217001", "35240000", "35260000", 
                                 "35279000", "35650000", "35668000", "35740000", 
                                 "35950000", "36020000", "36045000", "36110000", 
                                 "36125000", "36130000", "36160000", "36210000", 
                                 "36250000", "36270000", "36290000", "36470000", 
                                 "36520000", "35760000", "35875000", "35880000", 
                                 "36128000", "36180000", "36280000", "36460000", 
                                 "36550000"
                                 )
                            )

## Generating manual inventory
codes <- station_codes$y

estacoes_flu <- data.frame(station_code = codes,
                           stationType = "fluviometric")


## Downloading data
flu <- stationsData(inventoryResult = estacoes_flu, deleteNAstations = F)

## Creating flu datasource
for (x in 1:length(station_codes$x)){
  write.csv(assign(paste0("estacao_", station_codes$x[x]), flu[[x]]), file = paste0('datasource/est_fluviometricas_ana/', station_codes$x[x], '.csv'), row.names = F) 
}
