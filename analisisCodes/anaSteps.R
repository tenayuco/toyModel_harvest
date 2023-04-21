
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")

mycolsBW <- c("#000000", "#999999", "#FFFFFF")

DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
#DF_BASE <- read.csv("../../data/DF_total_TF.csv", header = TRUE)
DF_BASE <- DF_TOTAL


DF_BASE$DistanceW <- sqrt(DF_BASE$DistanceW)
maxLim <- 10

DF_BASE$LIM_1.5 <- (DF_BASE$DistanceW>1.5)*1
DF_BASE$LIM_VAR <- (DF_BASE$DistanceW>maxLim) *1
DF_BASE$CAT <- DF_BASE$LIM_1.5 +DF_BASE$LIM_VAR  ##describir categorias

DF_BASE$CAT[DF_BASE$CAT==0] <- "[0-1.5)"
DF_BASE$CAT[DF_BASE$CAT==1] <- paste("[1.5-", maxLim, ")",sep = "")
DF_BASE$CAT[DF_BASE$CAT==2] <- paste("[",maxLim,"-inf)",sep = "")


DF_BASE$contador <-1

DF_TOTAL_AG <- DF_BASE %>%
    group_by(HarvestModel, numPlants, numWorkers, Rust, Rep, HarvestTime, SimID, CAT) %>%
    summarise(sumCAT = sum(contador))

DF_TOTAL_AG$proCAT <- DF_TOTAL_AG$sumCAT/DF_TOTAL_AG$numPlants*100


FIG_PASOS <- DF_TOTAL_AG %>%
    filter(numPlants== 3000)%>%
    filter(Rust== 0.5)%>%
    #filter(HarvestModel == "closeness") %>%
    #filter(HarvestTime == 2)%>%
    #filter(CAT ==0)%>%
    ggplot(aes(x= CAT, y = proCAT)) +
    geom_boxplot(aes(fill = as.character(CAT)))+
    #geom_jitter(size= 1)+
    #  geom_line()+
    scale_fill_manual(values = mycols3a)+
    facet_wrap(~HarvestModel*HarvestTime, nrow = 2) +
    theme_bw() +
    theme(text = element_text(size = 20))+
    labs(x= "HarvestingTime", y= "% Rust-Effective Steps per size", fill= "SizeStep")
  
ggsave(FIG_PASOS,filename=paste("archivosTrabajandose/toyModelHarvest/output/saltosEfectivos/","Fig_pasosDIS", maxLim, ".png", sep=""),  height = 8, width = 14) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


###Lo que falta, correlacionarlo con la cantidad de small steps DEjar esto para pensar entre todo

  
DF_TOTAL_AG_EF <- DF_TOTAL_AG %>%
  filter(numPlants== 3000)%>%
  filter(Rust== 0.5)


DF_DIF<- DF_TOTAL_AG_EF  %>%
  #esto es para tener la maxima diferencia, pero tambien porque es aqui en donde pasa la cosecha
  group_by(Rep, numPlants, numWorkers, HarvestTime, CAT)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(difPro = diff(proCAT))  # esto es solo para que despues de leer por condicion, regrese...?

FIG_DIF <- DF_DIF %>%
  ggplot(aes(x= CAT, y = difPro)) +
  geom_boxplot(aes(fill = as.character(CAT)))+
  #geom_jitter(size= 1)+
  #  geom_line()+
  scale_fill_manual(values = mycols3a)+
  facet_wrap(~HarvestTime, nrow = 1) +
  geom_segment(aes(x=-1.5, y=0, xend= 3.5, yend=0), linewidth = 0.2, color= "DarkRed")+
  theme_bw() +
  theme(text = element_text(size = 20))+
  labs(x= "HarvestingTime", y= "% Rust-Effective Steps per size", fill= "SizeStep")


 # ggsave(FIG_PASOS,filename=paste("../../output/saltosEfectivos/", "Fig_pasos/","Fig_pasos", limiteSalto, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  