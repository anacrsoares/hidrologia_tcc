## Load libraries
library(raster)
library(sp)
library(rgdal)
library(os)
library(glue)

## List files from folder
ListOfFiles <- list.files(path = "C:/Users/Ana Carolina/Documents/tcc/sub-bacias/fazenda_boa_esperanca/gpm_imerg_tiff/", 
                         pattern = ".tif$", 
                         full.names = TRUE,
                         ignore.case = TRUE)

## Getting file's names
FileName <- list()
for (i in ListOfFiles) { 
  FileName <- c(FileName, toString(i))
}

## Getting file's date
ListOfDate <- list()
for (i in FileName) { 
  ListOfDate <- c(ListOfDate, substr(i, 
                                     start = unlist(gregexpr("3IMERG.", i))+7, 
                                     stop = unlist(gregexpr("3IMERG.", i))+14))
}

## Opening rasters files
ListOfRasters <- list()
for (i in ListOfFiles) { 
  ListOfRasters <- c(ListOfRasters, raster(i))
}


## Extracting data from rasters
DataRasters <- list()
for (i in 1:length(ListOfRasters)) {
  DataRasters <- c(DataRasters, data.frame(rasterToPoints(ListOfRasters[[i]])))
}


## Getting Precipitation Lists from DataRasters
Precipitation <- list()
PrecipitationData <- list()

for (i in ListOfDate) {
  Precipitation <- c(Precipitation, 
                     glue("X3B.DAY.MS.MRG.3IMERG.{i}.S000000.E235959.V06.nc4.SUB"))
}

for (i in Precipitation) {
  PrecipitationData[[i]] <- c(as.data.frame(DataRasters[[i]]))
}

AveragePrecipitation <- list()
for (i in PrecipitationData){
  AveragePrecipitation <- c(AveragePrecipitation, data.frame(mean(as.numeric(unlist(i)))))
}


## Create Dataframe Average
Date <- unlist(ListOfDate)
AveragePrecipitation <- unlist(AveragePrecipitation)

Precipitation.df <- data.frame(Date, AveragePrecipitation)

## Export CSV
write.csv(Precipitation.df,"C:/Users/Ana Carolina/Documents/tcc/sub-bacias/fazenda_boa_esperanca/gpm_imerg_csv//AveragePrecipitation.csv", row.names = FALSE)

