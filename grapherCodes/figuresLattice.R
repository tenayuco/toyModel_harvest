
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

#We take the full data frame of spatial average rust per condition. This average was the compounded average of each plant, In this sense,
# We have to run all this code from the terminal. If not, change the directory to an absolute path

DF_AV <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)
#DF_AV <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)

#we change control model to "no har"
DF_AV$porcionCosecha[DF_AV$HarvestModel =="control"] <- "control"

#DF_AV$HarvestTime[DF_AV$HarvestModel == "control"] <- " No_Har" 
DF_AV$numWorkers[DF_AV$HarvestModel =="control"] <- "NoH" 

DF_AV$porcionCosecha[DF_AV$porcionCosecha =="0.5"] <- "Asynchronic"
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="1"] <- "Synchronic" 


#procedimiento en donde duplicamos el control para tenerlo como si fuera un "escenario", pero realmente son los mismos datos
#desues de jacer el resumen h

RES_CON_1 <-  DF_AV %>% filter (HarvestModel == "control")
RES_CON_2 <- RES_CON_1 
RES_CON_1$porcionCosecha <-"Asynchronic" #esto es falso, no pertenecen ahi, pero es solo para graficar
RES_CON_2$porcionCosecha <-"Synchronic"

PART_RES_CON <- rbind(RES_CON_1,RES_CON_2)

DF_AV_MOD <- DF_AV %>% 
  filter(HarvestModel != "control" )

DF_AV_MOD <- rbind(DF_AV_MOD, PART_RES_CON)  ## so this framework adds a cnotrol conditions

rm("RES_CON_1", "RES_CON_2", "PART_RES_CON")  

DF_AV_MOD_AVR <- DF_AV_MOD %>%
  group_by(numPlants, numWorkers, HarvestModel, HarvestTime,porcionCosecha, Time, SimID) %>%
  summarise(AverageRust = mean(Rust), SD_Rust = sd(Rust))



for(nP in unique(DF_AV_MOD$numPlants)){


  
FIG_RUST <- DF_AV_MOD%>%
  filter(HarvestTime != 5)%>%
  filter(Time == 5)%>%
  #filter(Rust == 1)%>%
  filter(numPlants == nP)%>%
  ggplot(aes(x= as.character(HarvestTime), y= Rust))+
  geom_boxplot(aes(fill= as.character(numWorkers), group= interaction(numWorkers, HarvestTime)))+ 
  ggtitle("")+
  facet_wrap(~porcionCosecha, nrow=1)+
  scale_fill_manual(values = mycols3a)+
  theme_bw() +
  theme(text = element_text(size = 20))+
  ylim(0,1)+
  labs(x= "Time of Harvest", y= "Average Rust", col= "Number of Workers")

ggsave(FIG_RUST,filename=paste("../../output/graficas/RUST/", "rust_plants_", nP, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

FIG_SLI_time <- DF_AV_MOD_AVR %>%  ##FATLA POMERLA EL SD
  #filter(Rust == 1) %>%
  filter(numPlants == nP) %>%
  #filter(numWorkers ==3 | is.na(numWorkers))%>%  #da igial
  ggplot(aes(x= Time))+
  geom_line(aes(y= AverageRust, color= as.character(HarvestTime)))+
  geom_point(aes(y= AverageRust, color= as.character(HarvestTime)), size= 2)+
  geom_errorbar(aes(ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust, color= as.character(HarvestTime)), width=.2,
                position=position_dodge(0.05)) +
  ggtitle("")+
  facet_wrap(~porcionCosecha, ncol=2)+
  scale_color_manual(values = mycols)+
  theme_bw()+
  labs(x= "Time", y= "Average Rust", col= "Time of Harvest")
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SERIES/", "rust_time_", nP, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

}


  
  




#####################3




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
  theme(text = element_text(size = 20))+
  labs(x= "X", y= "Y", col= "Worker")

ggsave(FIG_PATH,filename=paste("../../output/graficas/PATH/", "path_plants_", nP, ".png", sep=""),  height = 10, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

}





##################################VNOW, WE ARE GOING TO USE THE NORMLA DF TO HAVE THE DIFFERENCE BETWEEN MODELS, OF THE SAME REPETITION ##########33

densidades = length(unique(DF_AV$numPlants))

DF_MODELS <- DF_AV %>%
  filter(HarvestModel != "control")%>%  #we remove the contorl, we do not need it. 
  filter(Time == 5)%>%
  group_by(Rep, numPlants, numWorkers, HarvestTime) %>%  #we add all the variables we dont add the ID, we remove it
  summarise(DifRust = diff(Rust)*(-1), PercIncrease = (-1)* 100*diff(Rust)/Rust[porcionCosecha=="Synchronic"])

FIG_DIF_MODELS <- DF_MODELS%>%
    #filter(numPlants ==3000) %>%  #CAHNGE THIS FOR THE AVERAGE!!!
    ggplot(aes(x= as.character(HarvestTime), y= PercIncrease))+
    geom_boxplot(aes(fill= as.character(numWorkers), group= interaction(numWorkers, HarvestTime)))+ 
    ggtitle("")+
    facet_wrap(~numPlants, nrow=1)+
    scale_fill_manual(values = mycols3a)+
    geom_segment(aes(x=0, y=0, xend= 6, yend=0), linewidth = 0.2, color= "DarkRed")+
    theme_bw() +
   theme(text = element_text(size = 20))+
    labs(x= "Time of Harvest", y= "% Increase (A-S)/S", fill= "Number of Workers")
  
ggsave(FIG_DIF_MODELS,filename="../../output/graficas/DIF_RUST/dif_rust.png",  height = 8, width = 6*densidades) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).






##############y aqui control vs tipos de cosecha (promediando todo el interior dentro de cada rep)

#DF_AV_TOTAL <- DF_AV %>%
 # filter(Time == 5)%>%
#  group_by(Rep, numPlants, HarvestModel) %>%
#  summarise(AV_Rust = mean(Rust))

DF_MODvsCON <- DF_AV %>%
  filter(Time == 5)%>% 
  filter(HarvestTime == 2 | HarvestTime == "NoH") %>%
  filter(numWorkers ==5 | numWorkers== "NoH")%>%
  #esto es para tener la maxima diferencia, pero tambien porque es aqui en donde pasa la cosecha
  group_by(Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(ModCo_Rust = 100*(Rust - Rust[HarvestModel=="control"]),
            porcionCosecha = porcionCosecha)  # esto es solo para que despues de leer por condicion, regrese...?

DF_MODvsCON$numPlants[DF_MODvsCON$numPlants == 500] <- " 500"

FIG_DIF_CONTROL <- DF_MODvsCON %>%
  filter(porcionCosecha != "control")%>%
  ggplot(aes(x= porcionCosecha, y= ModCo_Rust))+
  geom_boxplot(aes(fill= as.character(porcionCosecha)))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  facet_wrap(~numPlants, nrow=1)+
  scale_fill_brewer(palette = "Dark2")+ 
  #scale_fill_brewer(palette="YlOrRd") +
  #scale_fill_manual(values = mycols)+
  theme_bw() +
  theme(text = element_text(size = 20))+
  labs(x= "Porcion Cosecha", y= "% Rust (Scenario-Control)", fill= "PorcionCosecha")

ggsave(FIG_DIF_CONTROL,filename="../../output/graficas/DIF_RUST/dif_ModelvsControl22.png",  height = 8, width = 6*densidades) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


############33333s solo para que despues de leer por condicion, regrese...?

DF_MODvsCON_GEN <- DF_AV %>%
  filter(Time == 5)%>% 
  #filter(HarvestTime == 2 | HarvestTime == "NoH") %>%
  #filter(numWorkers ==5 | numWorkers== "NoH")%>%
  #esto es para tener la maxima diferencia, pero tambien porque es aqui en donde pasa la cosecha
  group_by(Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(ModCo_Rust = 100*((Rust - Rust[HarvestModel=="control"])/Rust[HarvestModel=="control"]),
            porcionCosecha = porcionCosecha, HarvestTime = HarvestTime, numWorkers = numWorkers)  # esto es solo para que despues de leer por condicion, regrese...?




FIG_DIF_CONTROL_MUL <- DF_MODvsCON_GEN %>%
  filter(porcionCosecha != "control")%>%
  ggplot(aes(x= as.character(HarvestTime), y= ModCo_Rust))+
  geom_boxplot(aes(fill= as.character(numWorkers)))+ 
  ggtitle("")+
  theme(text = element_text(size = 20))+
  facet_wrap(porcionCosecha~numPlants, nrow=2)+
  scale_fill_manual(values = mycols3a)+ 
  geom_segment(aes(x=0, y=0, xend= 6, yend=0), linewidth = 0.2, color= "DarkRed")+
  #scale_fill_brewer(palette="YlOrRd") +
  #scale_fill_manual(values = mycols)+
  theme_bw() +
  theme(text = element_text(size = 20))+
  labs(x= "Time of Harvest", y= "% Rust (Scenario-Control)/Control)", fill= "Number of Workers")



ggsave(FIG_DIF_CONTROL_MUL,filename="../../output/graficas/DIF_RUST/dif_ModelvsControl3.png",  height = 16, width = 6*densidades) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).




















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





#DOES NOT WORK WITH THE AVERAGE I DID
#HAR <- DF_AV %>% #esta agarra solo la cosecha por trabajador
#  filter(Time ==5) %>%
 # filter(HarvestTime != 5)%>%  ##Esto ya no existe en la nueva simulacion
  #filter(WorkerID !=0)%>%
  #filter(HarvestModel != "Control")%>%
  #group_by(HarvestModel, HarvestTime, numWorkers, Rep, numPlants, WorkerID)%>%  
  #summarise(CosechaTotalWorker = sum(TotalHarvest))



#HAR$CosechaTotal <- HAR$CosechaTotalWorker*HAR$numWorkers  #pensar si esto tiene sentido
# for(nP in unique(DF$numPlants)){
#   FIG_HAR_W <- HAR %>%
#     filter(numPlants == nP)%>%
#     ggplot()+
#     geom_boxplot(aes(x= as.character(HarvestTime), y= CosechaTotalWorker, color= as.character(numWorkers), group= interaction(numWorkers, HarvestTime)))+
#     ggtitle("")+
#     facet_wrap(~HarvestModel*numPlants, ncol=2)+
#     scale_color_manual(values = mycols)+
#     theme_bw()+
#     labs(x= "Time of Harvest", y= "Average Harvest per Worker", col= "Number of Workers")
#   
#   ggsave(FIG_HAR_W,filename=paste("../../output/graficas/HAR/", "harv_plants_", nP, ".png", sep=""),  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
#   
# }
