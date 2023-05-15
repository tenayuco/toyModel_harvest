
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycolsViridis <- c("#440154", "#3b528b", "#21918c", "#5ec962","#fde725","#111111")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#759580", "#fdb81c")
mycols3c <- c("#fdb81c", "#759580", "#1b4a64")
groupColors <- c("#1b4a64", '#56B4E9', "#759580", "#fd9706", "#fbdb30", "#111111")
groupColors2 <- c("#fd9706", "#021128",'#56B4E9', "#555555")
colorsDis <- c("#8B0000","#111111")
colorsDis3 <- c("#111111","#8B0000","#fd9706")



#This code plot the basic figures
#we will have a extrafilter for the plants equal to 2000, and for the additonal

#We take the full data frame of spatial average rust per condition. This average was the compounded average of each plant, In this sense,
# We have to run all this code from the terminal. If not, change the directory to an absolute path

#DF_AV <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)  #Run this with absolute path
DF_AV <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)

#we change control model to "no har"

DF_AV$porcionCosecha[DF_AV$HarvestModel =="control"] <- "NoH"
DF_AV$numWorkers[DF_AV$HarvestModel =="control"] <- "NoH" 
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="0.5"] <- "Asynchronic"
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="1"] <- "Synchronic" 

DF_AV$Rust <- DF_AV$Rust*100 #lo pasamos a un porcentage de infeccion para tener una intepretacion as sencilla 

#Our first verification is the number of lines for DF_AV
# we need to have a data frame with 
# [(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers) + (1 control)]* 5 numPlants * 21 (time max*2+1) times * 30 rep
#[21] * 5 * 21 * 30
# 66150  rows

#here we duplicate the control for each of the scenarios (synchronic, asyncrhn)

RES_CON_1 <-  DF_AV %>% filter (HarvestModel == "control")
RES_CON_2 <- RES_CON_1 
RES_CON_1$porcionCosecha <-"Asynchronic" #esto es falso, no pertenecen ahi, pero es solo para graficar
RES_CON_2$porcionCosecha <-"Synchronic"

PART_RES_CON <- rbind(RES_CON_1,RES_CON_2)

DF_AV_MOD <- DF_AV %>% 
  filter(HarvestModel != "control" )

DF_AV_MOD <- rbind(DF_AV_MOD, PART_RES_CON)  ## so this framework adds a cnotrol conditions

#This adds 1 control)]* 5 numPlants * 21 (time max*2+1) times * 30 rep ARTIFICIAL 
# 3150 
# so the total is 66150 + 3150 = 69300 rows for the DF_AV_MOD

rm("RES_CON_1", "RES_CON_2", "PART_RES_CON")  


#here we summarize or average between all repetition- So
#DF_AV_MOD_AVR should be 69300/30 = 2130 (combinations)

DF_AV_MOD_AVR <- DF_AV_MOD %>%
  group_by(numPlants, numWorkers, HarvestModel, HarvestTime,porcionCosecha, Time, SimID) %>%
  summarise(AverageRust = mean(Rust), SD_Rust = sd(Rust))

##################FIRST FIGURES, time seires, final time, control paarte
#these figures are only plotted without the number of workers that did not show a relevant difference
# so we are plotting only the last time == 10

timeMAX <- 10

FIG_RUST <- DF_AV_MOD%>%
  filter(Time == timeMAX)%>%
  filter(numWorkers !=5)%>%
  filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  ggplot(aes(x= as.character(HarvestTime), y= Rust))+
  geom_boxplot(color= "black", aes(fill= as.character(HarvestTime)))+ 
  ggtitle("")+
  facet_wrap(~porcionCosecha* numPlants, nrow=2)+
  scale_fill_manual(values = mycols)+
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "none")+
  ylim(0,100)+
  labs(x= "Date of Harvest", y= "Average Rust", fill= "Date of harvest")

ggsave(FIG_RUST,filename=paste("../../output/graficas/SUP_FIG/", "rust_plants_abs", ".png", sep=""),  height = 12, width = 18) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

FIG_SLI_time <- DF_AV_MOD_AVR %>% 
  filter(numWorkers !=5)%>% 
  filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  ggplot(aes(x= Time))+
  geom_line(linewidth= 1.5, aes(y= AverageRust, color= as.character(HarvestTime), linetype =HarvestModel))+
  #geom_point(aes(y= AverageRust, color= as.character(HarvestTime)), size= 2)+
  geom_errorbar(linewidth= 1.5, aes(ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust, color= as.character(HarvestTime)), width=.2,
                position=position_dodge(0.05)) +
  ggtitle("")+
  facet_wrap(~porcionCosecha* numPlants, nrow =2)+
  scale_color_manual(values = groupColors)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Time", y= "Average Rust", color= "Date of Harvest")
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SUP_FIG/", "rust_time_abs", ".png", sep=""),  height = 14, width = 20) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc)

### now we are going to see the differences between the model and control and between models for each repetition



densidades = length(unique(DF_AV$numPlants))

#this takes the Average and first remove all control rows. and put only time =10
# [(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers)]* 5 numPlants * * 30 rep
#[20] * 5 * 30
# 3000 rows 
# and then it groups by rep (30)*num plants(5) * num Wo (2)* Harvest tine (5) 1500, 
# so we do not include the harvest model in the gripuing, but we put the differeces between the models
# 1500 rows with a new variabe


DF_MODELS <- DF_AV %>%
  filter(HarvestModel != "control")%>%  #we remove the contorl, we do not need it. 
  filter(Time == timeMAX)%>%
  group_by(Rep, numPlants, numWorkers, HarvestTime) %>%  #we add all the variables we dont add the ID, we remove it
  summarise(DifRust = diff(Rust)*(-1), PercIncrease = (-1)* 100*diff(Rust)/Rust[porcionCosecha=="Synchronic"])

DF_MODELS$numPlants[DF_MODELS$numPlants == 500] <- " 500"

FIG_DIF_MODELS <- DF_MODELS%>%
  filter(numWorkers ==1)%>%
  filter(HarvestTime==2)%>%
  ggplot(aes(x= as.character(numPlants), y= DifRust))+
  geom_boxplot(color= "black", aes(fill= as.character(numPlants)))+ 
  ggtitle("")+
  #facet_wrap(~numPlants, nrow=2)+
  scale_fill_grey()+
  geom_segment(aes(x=0, y=0, xend= 6, yend=0), linewidth = 0.2, color= "DarkRed")+
  theme_bw() +
  theme(legend.position = "none")+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Density (Plants.ha)", y= "Rust Increase (A-S)", fill= "Density (Plants/ha)")

ggsave(FIG_DIF_MODELS,filename="../../output/graficas/DIF_RUST/dif_models.png",  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).




############33333s solo para que despues de leer por condicion, regrese...?

DF_MODvsCON_GEN <- DF_AV %>%
  filter(Time == timeMAX)%>% 
  group_by(Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(ModCo_Rust = ((Rust - Rust[HarvestModel=="control"])), Per_Rust = ((Rust - Rust[HarvestModel=="control"])/Rust[HarvestModel=="control"]),
            porcionCosecha = porcionCosecha, HarvestTime = HarvestTime, numWorkers = numWorkers)  # esto es solo para que despues de leer por condicion, regrese...?


FIG_DIF_CONTROL_MUL <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  filter(numWorkers == 1)%>%
  filter(numPlants == 2000) %>%
  ggplot(aes(x= as.character(HarvestTime), y= ModCo_Rust))+
  geom_boxplot(color = "black", aes(fill= as.character(porcionCosecha)))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  #facet_wrap(~numPlants, nrow=2)+
  scale_fill_manual(values = groupColors2)+ 
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Date of Harvest", y= "% Rust (Scenario-Control)", fill= "Coffee Maturation")

#ggsave(FIG_DIF_CONTROL_MUL,filename="../../output/graficas/DIF_RUST/ModelvsControl_timeHarvest.png",  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_DIF_CONTROL_H2 <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  filter(numWorkers == 1)%>%
  filter(HarvestTime== 0 | HarvestTime== 2 |HarvestTime== 4)%>%
  ggplot(aes(x= as.factor(numPlants), y= ModCo_Rust))+
  geom_boxplot(color = "black", aes(fill= porcionCosecha))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  facet_wrap(~HarvestTime, nrow=1)+
  scale_fill_manual(values = groupColors2)+ 
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  #theme(legend.position = "none")+
  labs(x= "Plants/ha", y= "% Rust (Scenario-Control)", fill= "Coffee maturation")

ggsave(FIG_DIF_CONTROL_H2,filename="../../output/graficas/DIF_RUST/ModelvsControl_harvest2_numW1.png",  height = 8, width = 20) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_DIF_CONTROL_ALL <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  filter(numWorkers != "NoH")%>%
  ggplot(aes(x= as.character(porcionCosecha), y= ModCo_Rust))+
  geom_boxplot(color = "black", aes(fill= interaction(as.character(numWorkers), porcionCosecha)))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  facet_wrap(~numPlants, nrow=1)+
  scale_fill_manual(values = groupColors)+ 
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "none")+
  labs(x= "Coffee maturation", y= "% Rust (Scenario-Control)", fill= "Coffee Maturation", title= "AverageHarvestTime")


ggsave(FIG_DIF_CONTROL_ALL,filename="../../output/graficas/SUP_FIG/ModelvsControl_allHar_numW1.png",  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


##################PATHS###############3

DF_SAM <- read.csv("../../data/DF_muestrasPath_complete.csv", header = TRUE)

#DF_SAM <-read.csv("archivosTrabajandose/toyModelHarvest/data/DF_muestrasPath_complete.csv")
#nP <- 2000
#nW <- "1 worker"
###########Plo

DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="0.5"] <- "Asynchronic"
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="1"] <- "Synchronic" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="1"] <- "1 worker" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="5"] <- "5 workers" 




FIG_PATH_GEN<- DF_SAM %>% 
    filter(Time == 5)%>% 
    filter(Rep == 0)%>%
    #filter(numWorkers ==nW)%>%
    filter(numPlants == 500 |numPlants == 2000 | numPlants == 5000)%>% #solo un ejepmplo
    filter(WorkerID != 0)%>% 
    arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
    rowwise() %>% 
    ggplot(aes(x= X, y = Y, group=WorkerID)) +
    geom_path(aes(col= WorkerID),linewidth=1)+
    geom_point(size=1)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
    #scale_color_viridis_c()+
    scale_color_manual(values = mycols)+
    facet_grid(numPlants~porcionCosecha*numWorkers) +
    theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white"))+ 
    theme(text = element_text(size = 20))+
    theme(legend.position = "None") +
    labs(x= "X", y= "Y", col= "Worker")
  
  ggsave(FIG_PATH_GEN,filename=paste("../../output/graficas/SUP_FIG/", "path_GENERAL.png", sep=""),  height = 12, width = 18) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
  

#############DISTRIBUCIOMES########################3

#DF_BASE <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
DF_BASE <- read.csv("../../data/DF_total_TF.csv", header = TRUE)
DF_BASE$DistanceW <- sqrt(DF_BASE$DistanceW)
DF_TOTAL = DF_BASE
DF_TOTAL$Conteo <- 1
#DF_TOTAL <- DF_TOTAL %>% filter(DistanceW > 1.5)  ##ESTO ES TRAMPA, perp vamos a ver
DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW,1) 
DF_TOTAL$porcionCosecha[DF_TOTAL$porcionCosecha =="0.5"] <- "Asynchronic"
DF_TOTAL$porcionCosecha[DF_TOTAL$porcionCosecha =="1"] <- "Synchronic" 
DF_TOTAL$Infection <- 0
DF_TOTAL$Infection[DF_TOTAL$Rust =="1"] <- "No Infection" 
DF_TOTAL$Infection[DF_TOTAL$Rust =="0"] <- "No Infection"
DF_TOTAL$Infection[DF_TOTAL$Rust =="0.5"] <- "New Infection"




FIG_PATH_2000_W1_V2<- DF_TOTAL %>% 
  filter(HarvestTime ==2)%>% 
  filter(Rep == 1)%>%
  filter(numWorkers ==1)%>%
  filter(numPlants == 2000)%>% #solo un ejepmplo
  filter(WorkerID != 0)%>% 
  arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
  rowwise() %>% 
  ggplot(aes(x= X, y = Y, group=WorkerID)) +
  geom_path(aes(col= as.character(Infection)),linewidth=1.5, alpha= 0.8)+
  geom_point(size=1.5)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  #scale_color_viridis_c()+
  scale_color_manual(values = colorsDis)+
  theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  theme_bw()+
  facet_wrap(~porcionCosecha, nrow = 2) +
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(text = element_text(size = 20))+
  theme(legend.position = "none") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+ 
  labs(x= "X", y= "Y", col= "Rust")

ggsave(FIG_PATH_2000_W1_V2,filename=paste("../../output/graficas/PATH/", "path_plants_2000_w1_V2.png", sep=""),  height = 10, width = 6) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

#

DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers, Rust,  Infection, DistanceW, HarvestTime, porcionCosecha, SimID) %>%
  summarise(FrecAbs =sum(Conteo))
DF_TOTAL_AG$Frec <- DF_TOTAL_AG$FrecAbs/(DF_TOTAL_AG$numPlants*length(unique(DF_TOTAL$Rep)))

#DF_PRUEBA_2 <- DF_TOTAL_AG %>% filter(SimID == 1 & Rep == 0)  #esto es solo para ver que lo este haciendo bien
#sum(DF_PRUEBA_2$Frec)

DF_TOTAL_AG$logDistanceW <- log(DF_TOTAL_AG$DistanceW)
DF_TOTAL_AG$logFrec <- log(DF_TOTAL_AG$Frec)

DF_TOTAL_AG_SHORT <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)%>%
  filter(numWorkers== 1) %>%
  filter(numPlants ==500 | numPlants== 2000 | numPlants==5000)


for (nP in unique(DF_TOTAL_AG_SHORT$numPlants)){
  for (pC in unique(DF_TOTAL_AG_SHORT$porcionCosecha)){

DF_TOTAL_TEMP <- DF_TOTAL_AG_SHORT %>%
  filter((numPlants== nP) & (porcionCosecha == pC)) %>%
  filter(HarvestTime== 2)    
      
FIG_PASOS_G <- DF_TOTAL_TEMP %>%
  ggplot(aes(x= DistanceW, y = Frec)) +
  geom_point(aes(color= as.character(Rust), alpha= 0.8), size=3.5)+
  #  geom_line()+
  xlim(0, 110)+
  scale_color_manual(values = colorsDis3)+
  #facet_wrap(~porcionCosecha, nrow = 2) +
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  scale_alpha(guide = 'none')+
  labs(color= "Rust", x= "Step length", y= "Proportion of Steps")

FIG_PASOS_C <- DF_TOTAL_TEMP %>%
  ggplot(aes(x= DistanceW, y = Frec)) +
  geom_point(aes(color= as.character(Rust), alpha= 0.8), size=3.5)+
  #  geom_line()+
  xlim(0, 5.1)+
  scale_color_manual(values = colorsDis3)+
  geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.01), linewidth = 0.2, color= "Black")+
  #facet_wrap(~porcionCosecha, nrow = 2) +
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "none")+
  labs(color= "Rust", x= "Step length", y= "Proportion of Steps")

FIG_INSIDE <- FIG_PASOS_G + annotation_custom(ggplotGrob(FIG_PASOS_C), xmin = 15, xmax = 80, ymin = max(DF_TOTAL_TEMP$Frec)/3, ymax = max(DF_TOTAL_TEMP$Frec))

ggsave(FIG_INSIDE,filename=paste("../../output/graficas/PATH/", "DisPasos_INSIDE_2", "nP", nP, "pC", pC, ".png", sep=""),  height = 8, width = 14) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

  }
}



FIG_PASOS_LOG <- DF_TOTAL_AG_SHORT %>%
  filter(HarvestTime ==0 | HarvestTime ==2 |HarvestTime ==4)  %>%  
  filter(numPlants== 2000 | numPlants== 500| numPlants== 5000)%>%
  ggplot(aes(x= logDistanceW, y = logFrec)) +
  geom_jitter(aes(color= as.character(Rust)), alpha=0.8, size= 1.5)+
  scale_color_manual(values = colorsDis3)+
  facet_grid(numPlants ~porcionCosecha*HarvestTime) +
  theme_bw() +
  geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), linewidth = 0.2, color= "Black")+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "log(Distance Step)", y= "log(Proportion of Steps)", color= "Rust")


ggsave(FIG_PASOS_LOG,filename=paste("../../output/graficas/PATH/", "logDisPasos_H2.png", sep=""),  height = 12, width = 18)



























# 
# 
# 
# 
# 
# 
# DF_TOTAL_AG$LIM_1.5 <- (DF_TOTAL_AG$DistanceW>1.5)*1
# 
# DF_TOTAL_AG$LIM_1.5[DF_TOTAL_AG$LIM_1.5 ==0] <- "=<1.5"
# DF_TOTAL_AG$LIM_1.5[DF_TOTAL_AG$LIM_1.5 ==1] <- ">1.5"
# 
# #vvamos a hacer el cambio agrupand sin categorias
# DF_SUM_AG <- DF_TOTAL_AG %>%
#   group_by(HarvestModel, numPlants, numWorkers, Infection, Rust, HarvestTime, porcionCosecha, SimID, LIM_1.5) %>%
#   summarise(FrecSum = sum(FrecAbs))
# 
# 
# DF_SUM_AG$FrecSumREL <-  (DF_SUM_AG$FrecSum /(DF_SUM_AG$numPlants*6))*2  #6 es por el num de reper
# 
# DF_SUM_AG_DIV <- DF_SUM_AG %>% 
#   group_by(HarvestModel, numPlants, numWorkers, LIM_1.5, HarvestTime, porcionCosecha, SimID) %>%
#   summarise(PropIvsN = (FrecSum[Rust == 0.5]/((FrecSum[Rust == 0])+ (FrecSum[Rust == 1]))))
# 
# 
# 
# 
# FIG_W_DEN <- DF_SUM_AG %>%
#   filter(HarvestTime ==2)  %>% 
#   #filter(Rust == "New Infection")%>%
#   filter(numWorkers == 1)%>%
#   filter(LIM_1.5 == ">1.5")%>%
#   ggplot(aes(x = numPlants, y = FrecSumREL))+
#   geom_line(aes(group = Rust))+
#  #stat_summary(fun = , na.rm = TRUE, color = 'darkblue', geom ='line')+
#   geom_point(aes(shape= as.character(Rust), color= as.character(Rust)), size= 3)+
#   scale_color_manual(values = colorsDis3)+
#   facet_wrap(~porcionCosecha, nrow = 1) +
#   theme_bw() +
#   #geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), linewidth = 0.2, color= "Black")+
#   theme(text = element_text(size = 20))+
#   theme(strip.background = element_rect(fill = "white"))+
#   theme(legend.position  = "none")+
#   labs(x= "numPlants", y= ">1.5 effective rust ", color= "RUST")
# 
# 
# FIG_W_DIV <- DF_SUM_AG_DIV %>%
#   filter(HarvestTime ==2)  %>% 
#   filter(LIM_1.5 == ">1.5")%>%
#   filter(numWorkers == 1)%>%
#   ggplot(aes(x = numPlants, y = PropIvsN))+
#   geom_line(aes())+
#   #stat_summary(fun = , na.rm = TRUE, color = 'darkblue', geom ='line')+
#   geom_point(size= 3)+
#   scale_color_manual(values = colorsDis)+
#   facet_wrap(porcionCosecha~HarvestTime, nrow = 1) +
#   theme_bw() +
#   #geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), linewidth = 0.2, color= "Black")+
#   theme(text = element_text(size = 20))+
#   theme(strip.background = element_rect(fill = "white"))+
#   #theme(legend.position  = "none")+
#   labs(x= "numPlants", y= ">1.5 effective rust ", color= "RUST")
# 
# 
# 
# 
# FIG_CAT <- DF_SUM_AG %>%
#   filter(HarvestTime ==2) %>%
#   filter(numWorkers== 1) %>%
#   filter(numPlants== 2000 | numPlants== 500| numPlants== 5000)%>%
#   ggplot(aes(x= as.character(LIM_1.5), y = FrecSumREL,fill = Rust)) +
#   geom_bar(stat = "identity", color= "black", alpha=0.8, width=0.4)+
#   scale_fill_manual(values = colorsDis)+
#   facet_grid(porcionCosecha ~numPlants) + 
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "white"))+ 
#   theme(text = element_text(size = 20))+
#   labs(x= "Step Size Category", y= "Proportion of Steps", color= "Rust")
# 
# 
# ggsave(FIG_CAT,filename=paste("../../output/graficas/PATH/", "CATDisPasos_H2.png", sep=""),  height = 8, width = 10)
# 



wholePlot <- 0

n= 0
if (wholePlot ==1){
  
  #DF_SAM = DF_sample
  
  for(nP in unique(DF_SAM$numPlants)){
    for(tiempo in c(0,timeMAX)){
      FIG_PLOT <- DF_SAM %>%
        filter(numPlants == nP)%>%
        filter(Rep ==0)%>%
        filter(Time == tiempo) %>%
        add_row(porcionCosecha = "Asynchronic",  Rep= 0,   ID= 999999,  X= NA,  Y=NA , Rust= 0.5)%>% ##ESte es un truquito para que siempre hayan 3 colores en cada graficas, porque le 0.5 siempre desaprece
        add_row(porcionCosecha = "Synchronic",  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Rust= 0.5)%>%
        ggplot()+
        geom_point(aes(x=X , y= Y, color= as.character(Rust)), size= 2)+
        ggtitle("")+
        facet_wrap(~porcionCosecha, ncol=2)+
        scale_color_manual(values = mycols3c)+
        theme(text = element_text(size = 20))+
        theme_bw()
      
      
      ggsave(FIG_PLOT,filename=paste("../../output/figurasLattice/",nP, "plotFigure_", tiempo, ".png",sep=""), height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
      n= n+1
    }
  }
}
