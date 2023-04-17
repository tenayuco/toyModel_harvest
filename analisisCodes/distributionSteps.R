
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


#this is the one to use first

DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
#DF_TOTAL <- read.csv("../../data/DF_total_TF.csv", header = TRUE)

DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW, 0) 
DF_TOTAL$Conteo <- 1

DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers, SimID, HarvestTime, Rust, DistanceW) %>%
  summarise(Frec =  sum(Conteo))

DF_TOTAL_AG$DistanceW <- sqrt(DF_TOTAL_AG$DistanceW)
DF_TOTAL_AG$logDistanceW <- log(DF_TOTAL_AG$DistanceW)
DF_TOTAL_AG$logFrec <- log(DF_TOTAL_AG$Frec)


#library(scales)

FIG_PASOS <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%
  filter(numPlants== 3000)%>%
  ggplot(aes(x= DistanceW, y = Frec, color= as.character(Rust))) +
  geom_point()+
  #  geom_line()+
  scale_color_manual(values = mycols3c)+
  facet_wrap(~HarvestModel*numWorkers)


FIG_PASOS_LOG <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%
  #filter(numPlants== 3000)%>%
  ggplot(aes(x= logDistanceW, y = logFrec, color= as.character(Rust))) +
  geom_point()+
  #  geom_line()+
  scale_color_manual(values = mycols3c)+
  facet_wrap(~HarvestModel*numPlants)

###########ahora sacamosss solo los saltos efectivos######333

DF_TOTAL_AG_EF <- DF_TOTAL_AG %>%
  filter(!(is.na(DistanceW)))%>%
  filter(DistanceW != 0) %>%
  filter(Rust ==0.5)#A VER SI ESTO TIENE SENTD

DF_RES <- DF_TOTAL_AG %>%
  group_by(HarvestModel, numPlants, numWorkers, SimID, HarvestTime)%>%
  summarise(DistanceW_Mean = mean(DistanceW, na.rm = TRUE))

DF_RES$Pendiente <- 0
DF_RES$Rsquared <- 0
DF_RES$PValue <- 0
DF_RES$CorP <- 0


for (sim in unique(DF_TOTAL_AG$SimID)){
  DF_TEMP <- DF_TOTAL_AG_EF %>% 
    filter(SimID== sim)
    lm1<- lm(logFrec ~ logDistanceW, data = DF_TEMP)
    summarylm1 <- summary(lm1)
    K <- lm1$coefficients[["logDistanceW"]]
    R <- summarylm1$adj.r.squared
    P <- summarylm1$coefficients[8]
    COR <- cor.test(abs(lm1$residuals), lm1$fitted.values)
    CP <- COR$p.value
    DF_RES$Pendiente[DF_RES$SimID ==sim] = K
    DF_RES$Rsquared[DF_RES$SimID ==sim] = R
    DF_RES$PValue[DF_RES$SimID ==sim] = P
    DF_RES$CorP[DF_RES$SimID ==sim] = CP
}


FIG_K_Treat <- DF_RES %>%
  filter(CorP>0.05)%>%
  ggplot(aes(x= HarvestTime, y = Rsquared)) +
  geom_point(size=3, pch=21, aes(shape= as.character(HarvestTime), fill= as.character(HarvestModel)))+
  scale_fill_manual(values = mycolsBW)+
  facet_wrap(~numWorkers*numPlants) +
  theme_bw()

FIG_K_R <- DF_RES %>%
  filter(CorP>0.05)%>%
  ggplot(aes(x= Pendiente, y = Rsquared)) +
  geom_point(size=3, pch=21, aes(shape= as.character(HarvestTime), fill= as.character(HarvestModel)))+
  scale_fill_manual(values = mycolsBW)+
  theme_bw()

###############ahora vmaos aintetar compararlo con RUST

DF_RUST <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)

DF_RUST <- DF_RUST %>%
  filter(Time ==5)%>%
  filter(HarvestModel != "control") %>%
  group_by(SimID)%>%
  summarise(MeanRust = mean(Rust))

DF_RES <- merge(DF_RES, DF_RUST, by= "SimID")  #esto esta bueno en general, aunque tengan cantidad es distintas, pega en cada id

FIG_Rust_K <- DF_RES %>%
  filter(CorP>0.05)%>%
  ggplot(aes(x= Pendiente, y = MeanRust)) +
  geom_point(size=3, pch=21, aes(fill= as.character(HarvestModel)))+
  scale_fill_manual(values = mycolsBW)+
  facet_wrap(~numPlants) +
  theme_bw()
