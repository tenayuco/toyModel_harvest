
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)

DF <- read.csv("../../data/DF_finalTime_complete.csv") #este es para correrlo desde la termunal
#DF <- read.csv("./archivosTrabajandose/toyModelHarvest/data/DF_finalTime_complete.csv")

DF$Total <- 1

##FALTA hacer en loop para cada densidad de pant
DF_ANOVA <- DF %>%
  filter(numPlants== 2000) %>% #variabl3e
  filter(numWorkers == 5)  %>%
  filter(HarvestModel != "control") %>%


  #hacerlo variable si es relevante, quedaria con mas factores..
  group_by(HarvestModel, HarvestTime, Rust)%>%
  summarise(Total_Sum = sum(Total)/(30*2000))%>%
  filter(Rust ==1)
