
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

#limiteSalto=25
#DF_BASE <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
DF_BASE <- read.csv("../../data/DF_total_TF.csv", header = TRUE)

##Aqui vamos a intentar filtrar los pasos chicos

#lo qur vamos a hacer es sacar todas las figuras y ver cuales tienen correl

DF_BASE$DistanceW <- sqrt(DF_BASE$DistanceW)
DF_BASE$DistanceW <- round(DF_BASE$DistanceW, 0) 

secuencia <- seq(5, max(DF_BASE$DistanceW, na.rm = TRUE), 5)
simulaciones <- length(secuencia)
DF_COR <- data.frame("SEC" = secuencia)
DF_COR$COR <-0 
DF_COR$COR_MIN <- 0
DF_COR$COR_MIN <- 0


for (limiteSalto in secuencia){
  DF_TOTAL = DF_BASE
  
  #DF_PRUEBA <- DF_TOTAL %>% filter(SimID == 1 & Rep == 0)  #solopara ver
  DF_TOTAL$Conteo <- 1
  DF_TOTAL <- DF_TOTAL %>% filter(DistanceW <= limiteSalto) 
  DF_TOTAL <- DF_TOTAL %>% filter(DistanceW > 1)  ##ESTO ES TRAMPA, perp vamos a ver

  DF_TOTAL_AG <- DF_TOTAL %>%
    group_by(HarvestModel, numPlants, numWorkers, Rust, DistanceW, Rep, HarvestTime, SimID) %>%
    summarise( FrecAbs =sum(Conteo))
  
  DF_TOTAL_AG$Frec <- DF_TOTAL_AG$FrecAbs/DF_TOTAL_AG$numPlants*100
  
  #DF_PRUEBA_2 <- DF_TOTAL_AG %>% filter(SimID == 1 & Rep == 0)  #esto es solo para ver que lo este haciendo bien
  #sum(DF_PRUEBA_2$Frec)
  
  DF_TOTAL_AG$logDistanceW <- log(DF_TOTAL_AG$DistanceW)
  DF_TOTAL_AG$logFrec <- log(DF_TOTAL_AG$Frec)
  
  

  FIG_PASOS <- DF_TOTAL_AG %>%
    filter(!is.na(DistanceW))%>%
    filter(DistanceW != 0)%>%
    #filter(HarvestModel == "productivity")%>%
    filter(numPlants== 3000)%>%
    filter(Rust== 0.5)%>%
    ggplot(aes(x= DistanceW, y = Frec)) +
    geom_point()+
    #  geom_line()+
    scale_color_manual(values = mycolsBW)+
    facet_wrap(~HarvestModel*HarvestTime, nrow = 2) +
    theme_bw() +
    labs(x= "Distance Step", y= "Proportion of Rust Effective Steps", title = paste("maxDistance", limiteSalto))
  
  
ggsave(FIG_PASOS,filename=paste("../../output/saltosEfectivos/", "Fig_pasos/","Fig_pasos", limiteSalto, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
  
  FIG_PASOS_LOG <- DF_TOTAL_AG %>%
    filter(!is.na(DistanceW))%>%
    filter(DistanceW != 0)%>%
    #filter(HarvestModel == "productivity")%>%
    filter(numPlants== 3000)%>%
    filter(Rust== 0.5)%>%
    ggplot(aes(x= logDistanceW, y = logFrec)) +
    geom_point()+
    #  geom_line()+
    scale_color_manual(values = mycolsBW)+
    facet_wrap(~HarvestModel*HarvestTime, nrow = 2) +
    theme_bw()+
    labs(x= "log(Distance Step)", y= "log(Proportion of Rust Effective Steps)", title = paste("maxDistance", limiteSalto))
  

  ggsave(FIG_PASOS_LOG,filename=paste("../../output/saltosEfectivos/", "Fig_pasos_log/","Fig_pasos_log", limiteSalto, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
  ###########ahora sacamosss solo los saltos efectivos######333
  
  DF_TOTAL_AG_EF <- DF_TOTAL_AG %>%
    filter(!(is.na(DistanceW)))%>%
    filter(DistanceW != 0) %>%
    filter(numPlants== 3000)%>%
    filter(Rust ==0.5)#A VER SI ESTO TIENE SENTD
  
  DF_RES <- DF_TOTAL_AG_EF %>%
    group_by(HarvestModel, numPlants, numWorkers, SimID, HarvestTime)%>%
    summarise(DistanceW_Mean = mean(DistanceW, na.rm = TRUE))
  
  DF_RES$Pendiente <- 0
  DF_RES$Rsquared <- 0
  DF_RES$PValue <- 0
  DF_RES$CorP <- 0
  
  
  for (sim in unique(DF_TOTAL_AG_EF$SimID)){
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
  
  
  
  FIG_K_R <- DF_RES %>%
    #filter(CorP>0.05)%>%
    ggplot(aes(x= Pendiente, y = Rsquared)) +
    geom_point(size=4, aes(shape= as.character(HarvestModel), color= as.character(HarvestTime)))+
    scale_color_manual(values = mycols)+
    ylim(0.5, 1)+
    theme_bw() +
    labs(x= "Slope (proportionEfe/distanceStep", y= "Rsquared", title = paste("maxDistance", limiteSalto))
  
 ggsave(FIG_K_R,filename=paste("../../output/saltosEfectivos/", "Fig_KR/","Fig_KR",  limiteSalto, ".png", sep=""),  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
  

  ###############ahora vmaos aintetar compararlo con RUST
  
  DF_RUST <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)
#  DF_RUST <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)
  
  DF_RUST <- DF_RUST %>%
    filter(Time ==5)%>%
    filter(numPlants ==3000)%>%
    filter(HarvestModel != "control") %>%
    group_by(SimID)%>%
    summarise(MeanRust = mean(Rust))
  
  DF_RES <- merge(DF_RES, DF_RUST, by= "SimID")  #esto esta bueno en general, aunque tengan cantidad es distintas, pega en cada id
  
  FIG_Rust_K <- DF_RES %>%
   # filter(CorP>0.05)%>%
    ggplot(aes(x= Pendiente, y = MeanRust)) +
    geom_point(size=4, aes(shape= HarvestModel, color= as.character(HarvestTime)))+
    scale_color_manual(values = mycols)+
    facet_wrap(~numPlants) +
    theme_bw() +
    labs(x= "Slope (proportionEfe/distanceStep", y= "MainRust", title = paste("maxDistance", limiteSalto))
  
  
  ggsave(FIG_Rust_K,filename=paste("../../output/saltosEfectivos/", "Fig_Rust_K/","Fig_Rust_K", limiteSalto, ".png", sep=""),  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
  
  rustNorm <- shapiro.test(DF_RES$MeanRust)
  slopeNorm <- shapiro.test(DF_RES$Pendiente)
  COR <- cor.test(DF_RES$MeanRust, DF_RES$Pendiente)
  DF_COR$COR[DF_COR$SEC == limiteSalto] <- COR$estimate[[1]]
  DF_COR$COR_MIN[DF_COR$SEC == limiteSalto] <- COR$conf.int[1]
  DF_COR$COR_MAX[DF_COR$SEC == limiteSalto] <- COR$conf.int[2]
  }


FIG_FINALE <- DF_COR %>%
  ggplot(aes(x= SEC, y = COR)) +
  geom_point(size=3, pch=21, aes(fill = "darkblue"))+
  geom_errorbar(aes(ymin= COR_MIN, ymax= COR_MAX))+
  ylim(0,1)+
  theme_bw() +
  labs(x= "limitMaxStep", y= "Cor (mainRust, Slope)")


ggsave(FIG_FINALE,filename=paste("../../output/saltosEfectivos/","Fig_FINALE", ".png", sep=""),  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

