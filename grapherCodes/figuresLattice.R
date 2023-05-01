
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycolsViridis <- c("#440154", "#3b528b", "#21918c", "#5ec962","#fde725","#111111")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")
groupColors <- c("#fd9706", "#fbdb30", "#021128", "#1b4a64",'#56B4E9', "#555555")
groupColors2 <- c("#fd9706", "#021128",'#56B4E9', "#555555")



#This code plot the basic figures

#We take the full data frame of spatial average rust per condition. This average was the compounded average of each plant, In this sense,
# We have to run all this code from the terminal. If not, change the directory to an absolute path

#DF_AV <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)  #Run this with absolute path
DF_AV <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)

#we change control model to "no har"

DF_AV$porcionCosecha[DF_AV$HarvestModel =="control"] <- "NoH"
DF_AV$numWorkers[DF_AV$HarvestModel =="control"] <- "NoH" 
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="0.5"] <- "Asynchronic"
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="1"] <- "Synchronic" 

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
  ylim(0,1)+
  labs(x= "Time of Harvest", y= "Average Rust", fill= "Time of harvest")

ggsave(FIG_RUST,filename=paste("../../output/graficas/RUST/", "rust_plants_abs", ".png", sep=""),  height = 12, width = 18) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

FIG_SLI_time <- DF_AV_MOD_AVR %>% 
  filter(numWorkers !=5)%>% 
  filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  ggplot(aes(x= Time))+
  geom_line(linewidth= 1, aes(y= AverageRust, color= as.character(HarvestTime), linetype =HarvestModel))+
  #geom_point(aes(y= AverageRust, color= as.character(HarvestTime)), size= 2)+
  geom_errorbar(linewidth= 1, aes(ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust, color= as.character(HarvestTime)), width=.2,
                position=position_dodge(0.05)) +
  ggtitle("")+
  facet_wrap(~porcionCosecha* numPlants, nrow =2)+
  scale_color_manual(values = mycolsViridis)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Time", y= "Average Rust", color= "Time of Harvest")
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SERIES/", "rust_time_abs", ".png", sep=""),  height = 14, width = 20) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc)

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

#we remove numWokers ==5
FIG_DIF_MODELS_PER <- DF_MODELS%>%
    filter(numWorkers !=5)%>%
    filter(numPlants == 2000) %>%
    ggplot(aes(x= as.character(HarvestTime), y= PercIncrease))+
    geom_boxplot(color= "black", aes(fill= as.character(numPlants), group= interaction(numWorkers, HarvestTime)))+ 
    ggtitle("")+
    facet_wrap(~numPlants, nrow=2)+
    scale_fill_grey()+
    geom_segment(aes(x=0, y=0, xend= 6, yend=0), linewidth = 0.2, color= "DarkRed")+
    theme_bw() +
    theme(legend.position = "none")+
   theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
    labs(x= "Time of Harvest", y= "% Increase (A-S)/S", fill= "Density (Plants/ha)")
  
ggsave(FIG_DIF_MODELS_PER,filename="../../output/graficas/DIF_RUST/perDif_models.png",  height = 8, width = 8) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_DIF_MODELS <- DF_MODELS%>%
  filter(numWorkers !=5)%>%
  ggplot(aes(x= as.character(HarvestTime), y= DifRust))+
  geom_boxplot(color= "black", aes(fill= as.character(numPlants), group= interaction(numWorkers, HarvestTime)))+ 
  ggtitle("")+
  facet_wrap(~numPlants, nrow=2)+
  scale_fill_grey()+
  geom_segment(aes(x=0, y=0, xend= 6, yend=0), linewidth = 0.2, color= "DarkRed")+
  theme_bw() +
  theme(legend.position = "none")+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Time of Harvest", y= "% Increase (A-S)/S", fill= "Density (Plants/ha)")

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
  labs(x= "Time of Harvest", y= "% Rust (Scenario-Control)", fill= "Coffee Maturation")

ggsave(FIG_DIF_CONTROL_MUL,filename="../../output/graficas/DIF_RUST/ModelvsControl_timeHarvest.png",  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_DIF_CONTROL_H2 <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  #filter(numWorkers == 1)%>%
  filter(HarvestTime== 2)%>%
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
  labs(x= "Coffee maturation", y= "% Rust (Scenario-Control)", fill= "numWorkers.coffeeMAT", title = "HarvestTime=2")

ggsave(FIG_DIF_CONTROL_H2,filename="../../output/graficas/DIF_RUST/ModelvsControl_harvest2_numW1.png",  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



FIG_DIF_CONTROL_H2_PER <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  filter(numWorkers == 1)%>%
  filter(HarvestTime== 2)%>%
  ggplot(aes(x= as.character(porcionCosecha), y= Per_Rust))+
  geom_boxplot(color = "black", aes(fill= as.character(porcionCosecha)))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  facet_wrap(~numPlants, nrow=1)+
  scale_fill_manual(values = groupColors)+ 
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "none")+
  labs(x= "Time of Harvest", y= "% Rust (Scenario-Control)/(Control)", fill= "Coffee Maturation", title = "HarvestTime=2")

ggsave(FIG_DIF_CONTROL_H2_PER,filename="../../output/graficas/DIF_RUST/ModelvsControl_harvest2_numW1PER.png",  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


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


ggsave(FIG_DIF_CONTROL_ALL,filename="../../output/graficas/DIF_RUST/ModelvsControl_allHar_numW1.png",  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).




##################PATHS###############3

DF_SAM <- read.csv("../../data/DF_muestrasPath_complete.csv", header = TRUE)

#DF_SAM <-read.csv("archivosTrabajandose/toyModelHarvest/data/DF_muestrasPath_complete.csv")
#nP <- 3000
###########Plo

DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="0.5"] <- "Asynchronic"
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="1"] <- "Synchronic" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="1"] <- "1 worker" 
DF_SAM$numWorkers[DF_SAM$numWorkers =="5"] <- "5 workers" 



for(nP in unique(DF_SAM$numPlants)){
  
  
  FIG_PATH<- DF_SAM %>% 
    filter(Time == 5)%>% 
    filter(Rep == 0)%>%
    filter(numPlants == nP)%>% #solo un ejepmplo
    filter(WorkerID != 0)%>% 
    arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
    rowwise() %>% 
    ggplot(aes(x= X, y = Y, group=WorkerID)) +
    geom_path(aes(col= WorkerID),linewidth=1)+
    geom_point(size=1)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
    #scale_color_viridis_c()+
    scale_color_manual(values = mycols)+
    facet_wrap(~numWorkers*porcionCosecha, ncol=2)+
    theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white"))+ 
    theme(text = element_text(size = 20))+
    labs(x= "X", y= "Y", col= "Worker")
  
  ggsave(FIG_PATH,filename=paste("../../output/graficas/PATH/", "path_plants_", nP, ".png", sep=""),  height = 10, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  
}

















wholePlot <- 0

n= 0
if (wholePlot ==1){

#DF_SAM = DF_sample

for(nP in unique(DF_SAM$numPlants)){
  for(tiempo in c(0,5)){
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




