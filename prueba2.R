library(tidyverse)
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)
datafrem <- read.csv(paste0(parentFolder
                             
                             ,"/electiva/data.csv")
                      
                      , stringsAsFactors = TRUE)
dataprueba<-data
dataprueba$Distancia <-
  as.numeric(data$Distancia)
dataprueba$Sensor <-
  as.numeric(data$Sensor)
colnames(dataprueba)
lm(Distancia,Sensor)

x<-data$Sensor
y<-data$Distancia
lm(y~x)

