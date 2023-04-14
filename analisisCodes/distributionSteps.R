
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")



#this is the one to use first

DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
#DF_TOTAL <- read.csv("../../data/DF_total_TF.csv", header = TRUE)



DF_TOTAL <- DF_TOTAL %>%
  filter(HarvestTime == 0)


DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW, 0) 
DF_TOTAL$Conteo <- 1


DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers, SimID, DistanceW) %>%
  summarise(Frec =  sum(Conteo))

DF_TOTAL_CONTROL <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers, SimID) %>%
  summarise(Frec =  sum(Conteo)) ##AQUI CORREGIR

library(scales)

FIG_PASOS <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%
  filter(numPlants== 3000)%>%
  ggplot(aes(x= sqrt(DistanceW), y = Frec, color= HarvestModel)) +
  geom_point()+
#  geom_line()+
  scale_color_manual(values = mycols3a)+
  facet_wrap(~HarvestModel*numWorkers)



FIG_PASOS_log <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%
  filter(numPlants== 3000)%>%
  ggplot(aes(x= log(sqrt(DistanceW)), y = log(Frec), color= HarvestModel)) +
  geom_point()+
  scale_color_manual(values = mycols3a)+
  facet_wrap(~HarvestModel*numWorkers)
