
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3c <- c("#fdb81c", "#759580", "#1b4a64")

groupColors2 <- c("#1b4a64", "#fd9706", "#fbdb30", "white")#ESTE ES CEMTAL
groupColors3 <- c("white", "#fd9706", "#1b4a64" )
groupColors <- c("#1b4a64", "#fd9706", "#fbdb30", "white", "#fd9706", "#fbdb30", "#111111")

groupColors4 <- c("#1b4a64", "#fd9706", "#fbdb30", "black")

colorsDis <- c("#B87400","white")
colorsDis2 <- c("#B87400","#555555")
colorsDis3 <- c("#111111","#8B0000","#fd9706")



#This code plot the basic figures
#we will have a extrafilter for the plants equal to 2000, and for the additonal

#We take the full data frame of spatial average rust per condition. This average was the compounded average of each plant, In this sense,
# We have to run all this code from the terminal. If not, change the directory to an absolute path

#DF_AV <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_spatialAverage_complete.csv", header = TRUE)  #Run this with absolute path
DF_AV <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)

#we change control model to "no har"

DF_AV$HarvestTime[DF_AV$HarvestModel =="control"] <- "control"
DF_AV$porcionCosecha[DF_AV$HarvestModel =="control"] <- "control"
#DF_AV$numWorkers[DF_AV$HarvestModel =="control"] <- "NoH" 


DF_AV$porcionCosecha[DF_AV$porcionCosecha =="A"] <- " Asynchronous"
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="S_F"] <- " Synchronous Final" 
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="S_I"] <- " Synchronous  Initial" 


DF_AV$HarvestModel[DF_AV$HarvestModel =="control"] <- "control"
DF_AV$HarvestModel[DF_AV$HarvestModel =="closeness"] <- "harvest"



DF_AV$Rust <- DF_AV$Rust*100 #lo pasamos a un porcentage de infeccion para tener una intepretacion as sencilla 


#Our first verification is the number of lines for DF_AV
# we need to have a data frame with 
# [(2 porcCosecha (A and S)* 1 harvestingTime * 2 numWorkers) + (1 control)]* 6 numPlants * 25 (time max*2+1) times * 30 rep
#[2 * 1 * 2 +1) * 5* 25* 30 rep
# 66150  rows


#here we summarize or average between all repetition- So
#DF_AV_MOD_AVR should be 66150/30 = 2205 (combinations)

DF_AV_AVR <- DF_AV %>%
  group_by(numPlants, numWorkers, HarvestModel, HarvestTime,porcionCosecha, Time, SimID) %>%
  summarise(AverageRust = mean(Rust), SD_Rust = sd(Rust))

##################FIRST FIGURES, time seires, final time, control paarte
#these figures are only plotted without the number of workers that did not show a relevant difference
# so we are plotting only the last time == 12

timeMAX <- 12


FIG_RUST <- DF_AV%>%
  filter(Time == timeMAX)%>%
  filter(numWorkers !=5)%>%
  #filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  filter(HarvestTime ==5 |HarvestTime =="control")%>% 
  mutate(porcionCosecha = (ifelse(porcionCosecha == "control", " Control", porcionCosecha)))%>% 
  ggplot(aes(x= as.factor(HarvestModel), y= Rust))+
  geom_boxplot(color= "black", aes(fill= as.character(porcionCosecha)))+ 
  ggtitle("")+
  facet_wrap(~ numPlants, nrow=1, labeller = label_both)+
  scale_fill_manual(values = groupColors)+
  theme_bw() +
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
 # theme(legend.position = "none")+
  ylim(0,100)+
  labs(x= "Density (Plants/ha)", y= "Average Rust", fill= "Coffee Maturation")

ggsave(FIG_RUST,filename=paste("../../output/graficas/SUP_FIG/", "rust_plants_abs", ".png", sep=""),  height = 10, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

#
FIG_SLI_time <- DF_AV_AVR %>% 
  filter(numWorkers !=5)%>% 
  filter(HarvestTime ==5|HarvestTime =="control")%>% 
  filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  
  mutate(porcionCosecha = (ifelse(porcionCosecha == "control", "No harvesting (control)", porcionCosecha)))%>% 
 # mutate(porcionCosecha = (ifelse(porcionCosecha == "0.5", "With large steps", porcionCosecha)))%>% 
#  mutate(porcionCosecha = (ifelse(porcionCosecha == "1", "Without large steps", porcionCosecha)))%>% 
  
  ggplot(aes(x= Time, color= as.character(porcionCosecha) ,
             fill= as.character(porcionCosecha)
             ))+
  geom_line(size= 1, aes(y= AverageRust))+
  #geom_point(aes(y= AverageRust, color= as.character(porcionCosecha)), size= 2)+
  geom_ribbon(color= "black", aes( 
                  ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust, linetype= as.character(porcionCosecha)),alpha=0.8) +
  ggtitle("")+
  facet_wrap(~ numPlants, nrow =1)+
  theme(panel.spacing = unit(1, "cm"))+
  
  scale_fill_manual(values = groupColors4)+
  scale_color_manual(values = groupColors4)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(panel.spacing = unit(1, "cm"))+
  #theme(legend.position = "none")
  labs(x= "Time step (t)", y= "Average Rust (%)", color= "Harvesting scenario", fill= "Harvesting scenario", linetype="Harvesting scenario" )
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SUP_FIG/", "rust_time_abs", ".png", sep=""),  height = 5, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc)

### now we are going to see the differences between the model and control and between models for each repetition



densidades = length(unique(DF_AV$numPlants))

#this takes the Average and first remove all control rows. and put only time =10
# [(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers)]* 5 numPlants * * 30 rep
#[20] * 5 * 30
# 3000 rows 
# and then it groups by rep (30)*num plants(5) * num Wo (2)* Harvest tine (5) 1500, 
# so we do not include the harvest model in the gripuing, but we put the differeces between the models
# 1500 rows with a new variabe


# DF_MODELS <- DF_AV %>%
#   filter(HarvestModel != "control")%>%  #we remove the contorl, we do not need it. 
#   filter(Time == timeMAX)%>%
#   group_by(Rep, numPlants, numWorkers, HarvestTime) %>%  #we add all the variables we dont add the ID, we remove it
#   summarise(DifRust = diff(Rust)*(-1), PercIncrease = (-1)* 100*diff(Rust)/Rust[porcionCosecha==1])
# 
# 
# FIG_DIF_MODELS <- DF_MODELS%>%
#   filter(numWorkers ==1)%>%
#   filter(numPlants!=4000)%>%
#   ggplot(aes(x= as.factor(numPlants), y= DifRust))+
#   geom_boxplot(color= "black", aes(fill= as.character(numPlants)))+ 
#   ggtitle("")+
#   #facet_wrap(~numPlants, nrow=2)+
#   scale_fill_grey()+
#   geom_segment(aes(x=0, y=0, xend= 6, yend=0), size = 0.2, color= "DarkRed")+
#   theme_bw() +
#   theme(legend.position = "none")+
#   theme(text = element_text(size = 25))+
#   theme(strip.background = element_rect(fill = "white"))+ 
#   labs(x= "Density (Plants/ha)", y= "Average Rust Difference (A-S)")
# 
# ggsave(FIG_DIF_MODELS,filename="../../output/graficas/DIF_RUST/dif_models.png",  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
# 



############33333s solo para que despues de leer por condicion, regrese...?


#aqui incluye 3000 renglones the time = harvestTime, que no tienen el control.
DF_MODvsCON_GEN <- DF_AV %>%
  filter(Time == HarvestTime |Time == timeMAX) %>%
  group_by(Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(Rust = Rust, ModCo_Rust = ((Rust - Rust[HarvestModel=="control"])), Per_Rust = ((Rust - Rust[HarvestModel=="control"])/Rust[HarvestModel=="control"]),
            porcionCosecha = porcionCosecha, HarvestTime = HarvestTime, numWorkers = numWorkers, Time= Time)  # esto es solo para que despues de leer por condicion, regrese...?


#esta deberia tener:

# [(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers) + 1 control que se filtra]* 5 numPlants * * 30 rep
#[21] * 5 * 30  = 3150 PARA EL HARVEST MAX#
#[(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers) 5 numPlants * * 30 rep
  #3000 para el harvest time. Esto porque aqui no se pone el control. 
  #6150
  

#quitamos esa basurita qui
DF_MODvsCON_GEN <- DF_MODvsCON_GEN%>%
filter(porcionCosecha != "control")




FIG_DIF_CONTROL_H2 <- DF_MODvsCON_GEN %>%
  filter(Time == timeMAX)%>%
 # filter(numWorkers == 1)%>%
  filter(numPlants!=4000)%>%
  mutate(porcionCosecha = (ifelse(porcionCosecha == "control", " No harvesting (control)", porcionCosecha)))%>% 
 # mutate(porcionCosecha = (ifelse(porcionCosecha == "0.5", "With large steps", porcionCosecha)))%>% 
#  mutate(porcionCosecha = (ifelse(porcionCosecha == "1", "Without large steps", porcionCosecha)))%>% 
  ggplot(aes(x= as.factor(HarvestTime), y= ModCo_Rust))+
  geom_boxplot(color = "black", aes(fill= porcionCosecha, linetype= porcionCosecha), position = "dodge")+ 
  ggtitle("")+
  facet_wrap(~numPlants, nrow = 1, strip.position = "bottom")+
  scale_fill_manual(values = groupColors2)+ 
  #scale_linetype_manual(values= c(2,4,1,5))+
  ylim(0, 18)+
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  #theme(strip.background = element_rect(fill = "white"))+ 
  theme(strip.background = element_blank())+
  theme(panel.spacing = unit(0, "cm"))+
  #theme(legend.position = "none")+
  labs(x= "Density (Plants/ha)", y= "Rust Increase (Scenario-Control)", fill= "Harvesting Scenario", linetype="Harvesting Scenario" )

ggsave(FIG_DIF_CONTROL_H2,filename="../../output/graficas/DIF_RUST/ModelvsControl_harvest4_numW1.png",  height = 5, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_DIF_CONTROL_ALL <- DF_MODvsCON_GEN %>%
  filter(Time == timeMAX)%>%
  filter(numPlants!=4000)%>%
  filter(porcionCosecha != "control")%>%
  filter(numWorkers != "NoH")%>%
  ggplot(aes(x= as.character(porcionCosecha), y= ModCo_Rust))+
  geom_boxplot(color = "black", aes(fill= interaction(as.character(numWorkers), porcionCosecha)))+ 
  ggtitle("")+
  theme(text = element_text(size = 25))+
  facet_wrap(~numPlants, nrow=1)+
  scale_fill_manual(values = groupColors)+ 
  theme_bw() +
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "none")+
  labs(x= "Coffee maturation", y= "% Rust (Scenario-Control)", fill= "Coffee Maturation", title= "AverageHarvestTime")


#ggsave(FIG_DIF_CONTROL_ALL,filename="../../output/graficas/SUP_FIG/ModelvsControl_allHar_numW1.png",  height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


#ahora viene la comparacion entre la diferencia maxima vs el control, en relacion con como estaba al inicio

DF_MODvsCON_GEN$SI <- (DF_MODvsCON_GEN$Rust/100)*(1- (DF_MODvsCON_GEN$Rust/100))

DF_MODvsCON_GEN$HarvestVsFinal <- 0
DF_MODvsCON_GEN$HarvestVsFinal[DF_MODvsCON_GEN$Time == 12] <- 1

DF_TEM_0 <- DF_MODvsCON_GEN %>%
filter(HarvestVsFinal ==0)

DF_TEM_1 <- DF_MODvsCON_GEN %>%
  filter(HarvestVsFinal ==1) 

DF_TEM_0$ModCo_Rust <- NULL
DF_TEM_0$Per_Rust <- NULL
DF_TEM_1$Rust <- NULL
DF_TEM_1$SI <- NULL
DF_FR_THR <- merge(DF_TEM_0, DF_TEM_1, by = c("Rep", "numPlants", "porcionCosecha", "HarvestTime", "numWorkers"))


FIGNUE <-DF_FR_THR %>% 
  filter(numWorkers == 1)%>%
  ggplot(aes(x= as.factor(numPlants) , y = SI))+
  geom_boxplot(color = "black", aes(fill= porcionCosecha))+ 
  ggtitle("")+
  facet_wrap(~HarvestTime, nrow=1)+
  scale_fill_manual(values = groupColors2)+ 
  theme_bw() +
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
  #theme(legend.position = "none")+
  labs(x= "Plants/ha", y= "S.I", fill= "Coffee maturation")

ggsave(FIGNUE, filename= "../../output/graficas/SUP_FIG/SIvsRust.png",  height = 8, width = 20) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).








##################PATHS###############3

#DF_SAM<- read.csv("./archivosTrabajandose/toyModelHarvest/data/DF_muestrasPath_complete.csv")
DF_SAM <- read.csv("../../data/DF_muestrasPath_complete.csv", header = TRUE)
#nP <- 2000
#nW <- "1 worker"
###########Plo



#DF_SAM$numWorkers[DF_SAM$numWorkers =="1"] <- "1 worker" 
#DF_SAM$numWorkers[DF_SAM$numWorkers =="5"] <- "5 workers" 


##aqui un filtro de la si y sf

DF_SAM_SI <- DF_SAM %>% filter(porcionCosecha == "S_I")
DF_SAM_SF <- DF_SAM %>% filter(porcionCosecha == "S_F")
DF_SAM_A <- DF_SAM %>% filter(porcionCosecha == "A")

DF_SAM_SI <- DF_SAM_SI %>%
  filter(HarvestStep < numPlants/(2*numWorkers)) # aver esto debe ser menor a 125 (la mitad de su jornada)

DF_SAM_SF <- DF_SAM_SF %>%
  filter(HarvestStep >= numPlants/(2*numWorkers)) # aver esto debe ser menor a 125 (la mitad de su jornada)

DF_SAM <- rbind(DF_SAM_A, DF_SAM_SF, DF_SAM_SI)

DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="A"] <- "Asynchronous"
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="S_I"] <- "Synchronous  Initial" 
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="S_F"] <- "Synchronous Final" 

FIG_PATH_GEN<- DF_SAM %>% 
    filter(Rep == 0)%>%
   # filter(WorkerID == "W_0")%>%
    filter(numPlants == 500 |numPlants == 2000 | numPlants == 5000)%>% #solo un ejepmplo
    filter(WorkerID != 0)%>% 
    arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
    rowwise() %>% 
    ggplot(aes(x= X, y = Y, group=WorkerID)) +
    geom_path(aes(col= WorkerID),size=1)+
    geom_point(size=1)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
    scale_color_viridis_d()+
    #scale_color_manual(values = mycols)+
    facet_grid(porcionCosecha ~numPlants) +
    theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
    theme_bw()+
    theme(strip.background = element_rect(fill = "white"))+ 
    theme(text = element_text(size = 25))+
    theme(legend.position = "none") +
    labs(x= "X", y= "Y", col= "Worker")
  
  ggsave(FIG_PATH_GEN,filename=paste("../../output/graficas/SUP_FIG/", "path_GENERAL.png", sep=""),  height = 12, width = 18) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  

#############DISTRIBUCIOMES########################3



DF_SAM$DistanceW <- sqrt(DF_SAM$DistanceW)

DF_TOTAL = DF_SAM %>%
  filter(Time ==5.5)

DF_TOTAL$Conteo <- 1
#DF_TOTAL <- DF_TOTAL %>% filter(DistanceW > 1.5)  ##ESTO ES TRAMPA, perp vamos a ver
DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW,1) 

DF_TOTAL$Infection <- 0
DF_TOTAL$Infection[DF_TOTAL$Rust =="1"] <- "No Infection" 
DF_TOTAL$Infection[DF_TOTAL$Rust =="0"] <- "No Infection"
DF_TOTAL$Infection[DF_TOTAL$Rust =="0.5"] <- "New Infection"

FIG_PATH_2000_W1_V1<- DF_TOTAL %>% 
  #filter(HarvestStep <160)%>% #ultimo 160 plantas de ahi
  filter(Rep == 2)%>%
 # filter(numWorkers =="1 worker")%>%
  filter(numPlants == 2000)%>% #solo un ejepmplo
  filter(WorkerID != 0)%>% 
  arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
  rowwise() %>% 
  ggplot(aes(x= X, y = Y, group=WorkerID)) +
  geom_path(aes(col= as.character(Infection)),size=1.5, alpha= 0.8)+
  geom_point(size=1.5)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  #scale_color_viridis_c()+
  scale_color_manual(values = colorsDis2)+
  theme(panel.spacing = unit(0.8, "lines"))+
  theme_bw()+
  facet_wrap(~porcionCosecha, nrow = 3) +
  theme(text = element_text(size = 20))+
  theme(legend.position = "none") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+ 
  labs(x= "X (m)", y= "Y (m)", col= "Rust")

ggsave(FIG_PATH_2000_W1_V1,filename=paste("../../output/graficas/PATH/", "path_plants_2000_w1_todos.png", sep=""),  height = 10, width = 4.5) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).




FIG_PATH_2000_W1_V2<- DF_TOTAL %>% 
  #filter(HarvestStep <160)%>% #ultimo 160 plantas de ahi
  filter(Rep == 2)%>%
  # filter(numWorkers =="1 worker")%>%
  filter(numPlants ==2000)%>% #solo un ejepmplo
  filter(WorkerID == "W_2")%>% 
  arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
  rowwise() %>% 
  ggplot(aes(x= X, y = Y, group=WorkerID)) +
  geom_path(aes(col= as.character(Infection)),size=1.5, alpha= 0.8)+
  geom_point(size=1.5)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  #scale_color_viridis_c()+
  scale_color_manual(values = colorsDis2)+
  theme(panel.spacing = unit(0.8, "lines"))+
  theme_bw()+
  facet_wrap(~porcionCosecha, nrow = 3) +
  theme(text = element_text(size = 20))+
  theme(legend.position = "none") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+ 
  labs(x= "X (m)", y= "Y (m)", col= "Rust")

ggsave(FIG_PATH_2000_W1_V2,filename=paste("../../output/graficas/PATH/", "path_plants_3000_1Worker.png", sep=""),  height = 10, width = 4.5) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


#
#DF_PRUEBA_1 <- DF_TOTAL %>%
 # filter(SimID == 1)

DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers,  Infection, DistanceW, HarvestTime, porcionCosecha, SimID) %>%
  summarise(FrecAbs =sum(Conteo))

#el numero de lineas depende de cuanta distancias difeentes tenga

DF_TOTAL_AG$Frec <- 2* DF_TOTAL_AG$FrecAbs/(DF_TOTAL_AG$numPlants*length(unique(DF_TOTAL$Rep)))

#DF_PRUEBA_2 <- DF_TOTAL_AG %>% filter(SimID == 1)  #esto es solo para ver que lo este haciendo bien
#sum(DF_PRUEBA_2$Frec) #esta suma debe ser plantas/2 *numrepe, la abs debe ser igual a 1

DF_TOTAL_AG$logDistanceW <- log(DF_TOTAL_AG$DistanceW)
DF_TOTAL_AG$logFrec <- log(DF_TOTAL_AG$Frec)

DF_TOTAL_AG_SHORT <- DF_TOTAL_AG %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)


for (nP in unique(DF_TOTAL_AG_SHORT$numPlants)){
  for (pC in unique(DF_TOTAL_AG_SHORT$porcionCosecha)){

DF_TOTAL_TEMP <- DF_TOTAL_AG_SHORT %>%
  filter((numPlants== nP) & (porcionCosecha == pC))
      
FIG_PASOS_G <- DF_TOTAL_TEMP %>%
  ggplot(aes(x= DistanceW, y = Frec)) +
  geom_point(aes(fill= as.character(Infection)), color= "black",  shape=21, size=5, stroke=1, alpha= 0.7)+
  xlim(0, 110)+
  ylim(0, 0.05)+
  scale_fill_manual(values =colorsDis)+
  theme_bw() +
  theme(text = element_text(size = 30))+
  theme(strip.background = element_rect(fill = "white"))+ 
  scale_alpha(guide = 'none')+
  labs(fill= "Rust", x= "Step length (m)", y= "Proportion of Steps")

FIG_PASOS_C <- DF_TOTAL_TEMP %>%
  ggplot(aes(x= DistanceW, y = Frec)) +
  geom_point(aes(fill= as.character(Infection)), color= "black",  shape=21, size=5, stroke=1,alpha= 0.7)+
  xlim(0, 7.1)+
  ylim(0, 0.05)+
  scale_fill_manual(values = colorsDis)+
  geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.05), size = 0.2, color= "Black")+
  annotate(geom="text", x=4.75, y=0.04, label="Max. contact \ndispersal distance",
           color="black",
           size =8)+
  geom_segment(aes(x=2, y=0.04, xend=1.52, yend=0.04-0.001), 
               arrow = arrow(length=unit(.2, 'cm')))+
    theme_bw() +
  theme(text = element_text(size = 30))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "None")+
  theme(axis.title.x=element_blank(), #remove x axis labels
        axis.title.y=element_blank(),  #remove y axis labels
  )
  #labs(fill= "Rust", x= "Step length", y= "Proportion of Steps")

FIG_INSIDE <- FIG_PASOS_G + annotation_custom(ggplotGrob(FIG_PASOS_C), xmin = 15, xmax = 80, ymin = max(DF_TOTAL_TEMP$Frec)/3, ymax = max(DF_TOTAL_TEMP$Frec))

ggsave(FIG_INSIDE,filename=paste("../../output/graficas/PATH/", "DisPasos_INSIDE_", "nP_", nP, "pC_", pC, ".png", sep=""),  height = 8, width = 14) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

  }
}



FIG_PASOS_LOG <- DF_TOTAL_AG_SHORT %>%
 # filter(Rust == 0.5)%>%  
  filter(numPlants== 2000 | numPlants== 500| numPlants== 5000)%>%
  ggplot(aes(x= logDistanceW, y = logFrec)) +
  geom_jitter(aes(fill= as.character(Infection)), color= "black",  shape=21, size=1.5, stroke=1,alpha= 0.7)+
  scale_fill_manual(values = colorsDis)+
  facet_grid(porcionCosecha ~numPlants) +
  theme_bw() +
  geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), size = 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "ln(Step length) (m)", y= "ln(Proportion of Steps)", color= "Rust")


ggsave(FIG_PASOS_LOG,filename=paste("../../output/graficas/PATH/", "logDisPasos_H2.png", sep=""),  height = 12, width = 18)


FIG_PASOS <- DF_TOTAL_AG_SHORT %>%
  #filter(Rust == 0.5)%>%  
  filter(DistanceW >= 1.5)%>%  
  filter(numPlants== 2000 | numPlants== 500| numPlants== 5000)%>%
  ggplot(aes(x= DistanceW, y = Frec)) +
  geom_jitter(aes(color= as.character(Infection)), alpha=0.7, size= 1.5)+
  geom_smooth()+
  scale_color_manual(values = colorsDis)+
  xlim(0,5)+
  facet_grid(porcionCosecha ~numPlants) +
  theme_bw() +
  #geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.025, size = 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
  labs(x= "Step length (m)", y= "Proportion of Steps", color= "Rust")

ggsave(FIG_PASOS,filename=paste("../../output/graficas/PATH/", "DisPasos_total.png", sep=""),  height = 12, width = 18)

# 
# 
DF_TOTAL_AG$LIM_1.5 <- (DF_TOTAL_AG$DistanceW>=1.5)*1

DF_TOTAL_AG$LIM_1.5[DF_TOTAL_AG$LIM_1.5 ==0] <- "<1.5"
DF_TOTAL_AG$LIM_1.5[DF_TOTAL_AG$LIM_1.5 ==1] <- ">=1.5"

#vvamos a hacer el cambio agrupand sin categorias
DF_SUM_AG <- DF_TOTAL_AG %>%
  group_by(HarvestModel, numPlants, numWorkers, Infection, HarvestTime, porcionCosecha, SimID, LIM_1.5) %>%
  summarise(FrecSum = sum(FrecAbs))

#DF_PRUEBA_3 <- DF_SUM_AG %>%
 # filter(HarvestTime ==4 & Rust == 0.5 & LIM_1.5 == ">=1.5" & numWorkers ==1)

DF_SUM_AG$FrecSumREL <-  (DF_SUM_AG$FrecSum /(DF_SUM_AG$numPlants*length(unique(DF_TOTAL$Rep))))*2  #6 es por el num de reper

#DF_SUM_AG_DIV <- DF_SUM_AG %>%
 # group_by(HarvestModel, numPlants, numWorkers, LIM_1.5, HarvestTime, porcionCosecha, SimID) %>%
#  summarise(PropIvsN = (FrecSum[Rust == 0.5] * (FrecSum[Rust == 0]+FrecSum[Rust == 1])))



FIG_W_DEN <- DF_SUM_AG %>%
  filter(Infection == "New Infection")%>%
  #filter(numWorkers == "1 worker")%>%
  filter(LIM_1.5 == ">=1.5")%>%
 # mutate(porcionCosecha = (ifelse(porcionCosecha == "Asynchronous", "With large steps", porcionCosecha)))%>% 
  #mutate(porcionCosecha = (ifelse(porcionCosecha == "Synchronous", "Without large steps", porcionCosecha)))%>% 
  ggplot(aes(x = numPlants, y = FrecSumREL))+
  geom_line(aes(group = porcionCosecha))+
 #stat_summary(fun = , na.rm = TRUE, color = 'darkblue', geom ='line')+
  geom_point(aes(shape= as.character(porcionCosecha), color= as.character(porcionCosecha)), size= 3)+
  scale_color_manual(values = groupColors2)+
  #facet_wrap(~Rust, nrow = 1) +
  theme_bw() +
  #geom_segment(aes(x=log(1.5), y=-11, xend= log(1.5), yend=-2), size= 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+
#  theme(legend.position  = "none")+
  labs(x= "Density (Plants/ha)", y= "Proportion of plants", color= "Harvesting Scenario",shape= "Harvesting Scenario")

ggsave(FIG_W_DEN,filename=paste("../../output/graficas/SUP_FIG/", "FIG_W_DEN.png", sep=""),  height = 6, width = 12)


#######################cumulativo a ver########################33

DF_ARRANGED <- DF_TOTAL %>%
  filter(!is.na(DistanceW))%>%
 # filter(Rep ==2)%>%
  #filter(WorkerID == "W_2")%>%
  filter(DistanceW != 0)%>%  #aqui perdemos un porcentaje del total
  #filter(numWorkers== "1 worker")%>%
  select(HarvestStep, DistanceW, Rep, numPlants, HarvestModel, porcionCosecha, WorkerID, Infection)%>%
  dplyr::mutate(pasoLargo =   round(as.integer(DistanceW/25)/(DistanceW/25))) #truco para que todo valga 1 si es mayor a 18 reportado así como salto laog

DF_CUM <- DF_ARRANGED %>%
  group_by(Rep, numPlants, HarvestModel, porcionCosecha)%>%
  arrange(numPlants, HarvestModel, porcionCosecha, Rep, HarvestStep, WorkerID) %>% 
  dplyr::mutate(cs = cumsum(DistanceW))%>% 
  dplyr::mutate(cumPasoLargo = cumsum(pasoLargo))


#DF_CUM$PerStep <- 100*DF_CUM$HarvestStep/250

#
DF_CUM_A <- DF_CUM %>%
  filter(porcionCosecha == "Asynchronous") %>%
  mutate(PerStep = 100*HarvestStep/250)

DF_CUM_SI <- DF_CUM%>%
  filter(porcionCosecha == "Synchronous  Initial")%>%
  mutate(PerStep = 100*HarvestStep/250)

DF_CUM_SF <- DF_CUM%>%
  filter(porcionCosecha == "Synchronous Final")%>%
  mutate(PerStep = 100*HarvestStep/250-100)


DF_CUM <- rbind(DF_CUM_A, DF_CUM_SI, DF_CUM_SF)


rm(DF_CUM_A)
rm(DF_CUM_SI)
rm(DF_CUM_SF)

DF_CUM$ArbNoCos <- DF_CUM$numPlants - DF_CUM$HarvestStep

DF_CUM$PerCos <- 100 -(DF_CUM$ArbNoCos/DF_CUM$numPlants *100)


FIG_CUM <- DF_CUM %>%
  #filter(Rep ==2)%>%
  #filter(WorkerID == "W_2")%>%
  #filter(numPlants== 2000)%>%
  ggplot(aes(x= PerStep, y = DistanceW)) +
  geom_line(aes(color= as.factor(Rep), group= WorkerID))+
  facet_grid(porcionCosecha ~numPlants) +
  theme_bw() +
  scale_color_viridis_d()+
  #geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.025, size = 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
  theme(legend.position = "None")+
  labs(x= "% Harvested trees", y= "Size of step per worker")


ggsave(FIG_CUM,filename=paste("../../output/graficas/SUP_FIG/", "FIG_PROGRESSION.png", sep=""),  height = 10, width = 20)



FIG_CUM_2000 <- DF_CUM %>%
  filter(Rep ==2)%>%
 # filter(WorkerID == )%>%
  filter(numPlants== 2000)%>%
  ggplot(aes(x= PerStep, y = DistanceW)) +
  geom_line(aes(group = WorkerID, color = Infection))+
  facet_wrap(~porcionCosecha, nrow = 3) +
  theme_bw() +
  scale_color_manual(values = colorsDis2)+
  #geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.025, size = 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  labs(x= "% Harvested trees", y= "Size of step per worker", color= "Rust")


ggsave(FIG_CUM_2000,filename=paste("../../output/graficas/PATH/", "FIG_PROGRESSION_2000.png", sep=""),  height = 12, width = 9 )


#FIG_CUM_PASOSL <- DF_CUM %>%
 # filter(Rep ==2)%>%
  #filter(numPlants== 2000) %>%
  #filter(numPlants== 2000)%>%
  # ggplot(aes(x= PerCos, y = cumPasoLargo, col= porcionCosecha)) +
  # geom_line()+
  # facet_grid(~numPlants) +
  # theme_bw() +
  # #geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.025, size = 0.2, color= "Black")+
  # theme(text = element_text(size = 25))+
  # theme(strip.background = element_rect(fill = "white"))+ 
  # labs(x= "% Trees without coffee", y= "Number of large steps (>18 m)", color= "Fruit Maturation")
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
        add_row(porcionCosecha = "Asynchronous",  Rep= 0,   ID= 999999,  X= NA,  Y=NA , Rust= 0.5)%>% ##ESte es un truquito para que siempre hayan 3 colores en cada graficas, porque le 0.5 siempre desaprece
        add_row(porcionCosecha = "Synchronous",  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Rust= 0.5)%>%
        ggplot()+
        geom_point(aes(x=X , y= Y, color= as.character(Rust)), size= 2)+
        ggtitle("")+
        facet_wrap(~porcionCosecha, ncol=2)+
        scale_color_manual(values = mycols3c)+
        theme(text = element_text(size = 25))+
        theme_bw()
      
      
      ggsave(FIG_PLOT,filename=paste("../../output/figurasLattice/",nP, "plotFigure_", tiempo, ".png",sep=""), height = 8, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
      n= n+1
    }
  }
}
