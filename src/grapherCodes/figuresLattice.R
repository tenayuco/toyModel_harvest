## ------------------------------------------------------------------------------------
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



## ------------------------------------------------------------------------------------
#DF_SAM<- read.csv("./archivosTrabajandose/toyModelHarvest/data/DF_muestrasPath_complete.csv")
DF_SAM <- read.csv("../../data/DF_muestrasPath_complete.csv", header = TRUE)
#nP <- 2000
#nW <- "1 worker"
###########Plo


DF_SAM_SI <- DF_SAM %>% filter(porcionCosecha == "S_I")
DF_SAM_SF <- DF_SAM %>% filter(porcionCosecha == "S_F")
DF_SAM_A <- DF_SAM %>% filter(porcionCosecha == "A")

DF_SAM_SI <- DF_SAM_SI %>%
  filter(HarvestStep < numPlants/(2*numWorkers)) # aver esto debe ser menor a 125 (la mitad de su jornada)

DF_SAM_SF <- DF_SAM_SF %>%
  filter(HarvestStep >= numPlants/(2*numWorkers)) # aver esto debe ser menor a 125 (la mitad de su jornada)

DF_SAM <- rbind(DF_SAM_A, DF_SAM_SF, DF_SAM_SI)


#aqui quitamos el SF, para la version más sencilla 


DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="S_F"] <- "Synchronous Final" 


DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="A"] <- "Asynchronous"
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="S_I"] <- "Synchronous  Initial" 
DF_SAM$porcionCosecha[DF_SAM$porcionCosecha =="S_F"] <- "Synchronous Final" 

DF_SAM <- DF_SAM %>%
  filter(porcionCosecha != "Synchronous Final")

DF_SAM$DistanceW <- sqrt(DF_SAM$DistanceW)  #esto porque estaba al cuadrado los datos




## ------------------------------------------------------------------------------------

DF_TOTAL = DF_SAM %>%
  filter(Time ==5.5)

rep <- length(unique(DF_TOTAL$Rep))


DF_TOTAL$Conteo <- 1
#prueba s
DF_TOTAL$DistanceW <- sqrt(DF_TOTAL$DistanceW) 



DF_TOTAL <- DF_TOTAL %>%
  filter(!is.na(DistanceW))%>%
  filter(DistanceW != 0)

DF_TOTAL$DistanceW <- round(DF_TOTAL$DistanceW,1)  


####3
DF_TOTAL$Infection <- 0
DF_TOTAL$Infection[DF_TOTAL$Rust =="1"] <- "No Infection" 
DF_TOTAL$Infection[DF_TOTAL$Rust =="0"] <- "No Infection"
DF_TOTAL$Infection[DF_TOTAL$Rust =="0.5"] <- "New Infection"



DF_TOTAL_PROXY <- DF_TOTAL %>%
  filter(porcionCosecha == "Asynchronous")%>%
  filter(numPlants == 2000)%>%
  filter(Infection == "New Infection")%>%
  filter(Rep %in% c(0,1,2,3,4,5))%>%
  filter(DistanceW <1.1)


#DF_TOTAL <- DF_TOTAL_PROXY


## ------------------------------------------------------------------------------------
FIG_PATH_GEN<- DF_TOTAL %>% 
    filter(Rep == 0)%>%
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



## ------------------------------------------------------------------------------------
FIG_PATH_2000_todos<- DF_TOTAL %>% 
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

ggsave(FIG_PATH_2000_todos,filename=paste("../../output/graficas/PATH/", "path_plants_2000_w1_todos.png", sep=""),  height = 10, width = 4.5) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).




## ------------------------------------------------------------------------------------
FIG_PATH_2000_W1<- DF_TOTAL %>% 
  #filter(HarvestStep <160)%>% #ultimo 160 plantas de ahi
  filter(Rep == 2)%>%
  # filter(numWorkers =="1 worker")%>%
  filter(numPlants ==2000)%>% #solo un ejepmplo
  filter(WorkerID == "W_1")%>% 
  arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
  rowwise() %>% 
  ggplot(aes(x= X, y = Y, group=WorkerID)) +
  geom_path(col= "black",size=1.5, alpha= 0.7)+
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

ggsave(FIG_PATH_2000_W1,filename=paste("../../output/graficas/PATH/", "path_plants_2000_1Worker.png", sep=""),  height = 10, width = 4.5) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



## ------------------------------------------------------------------------------------
DF_TOTAL_AG <- DF_TOTAL %>%
  group_by(HarvestModel, numPlants, numWorkers,  Infection, DistanceW, HarvestTime, porcionCosecha, SimID, Rep) %>%
  summarise(FrecAbs =sum(Conteo))

DF_TOTAL_AG$Frec <- 2* DF_TOTAL_AG$FrecAbs/(DF_TOTAL_AG$numPlants)
#el 2 es porque realmente es la mitad del numbero de plantas



DF_TOTAL_AG <- DF_TOTAL_AG %>%
  group_by(HarvestModel, numPlants, numWorkers,  Infection, DistanceW, HarvestTime, porcionCosecha, SimID) %>%
  summarise(MeanFrec =sum(Frec)/rep)


DF_TOTAL_AG <- DF_TOTAL_AG %>%
  group_by(Infection, numPlants, porcionCosecha) %>%
  mutate(FrecCUM = cumsum(MeanFrec))


## ------------------------------------------------------------------------------------
DF_TOTAL_TEMP <- DF_TOTAL_AG %>%
  filter((numPlants== 2000) & (porcionCosecha == "Synchronous  Initial"))

#DF_TOTAL_TEMP <- DF_TOTAL_TEMP %>%
 # group_by(Infection) %>%
  #mutate(FrecCUM = cumsum(MeanFrec))

##aqui un truco para no graficar el que no genera nueva infectin para la escala

DF_TOTAL_TEMP$FrecCUM[DF_TOTAL_TEMP$Infection == "No Infection"] <- NA


FIG_HIST_G <- DF_TOTAL_TEMP %>%
  #filter(Infection == "New Infection")%>%
  ggplot(aes(x= DistanceW)) +
  geom_col(aes(y = MeanFrec, fill= as.character(Infection)),
             color= "black", position = "stack")+
 # geom_histogram(color= "black", aes(fill= as.character(Infection), y = after_stat(count / sum(count))),binwidth=1) +
  
  geom_line(aes(y= FrecCUM, group= as.character(Infection)), color= "black",  size=2)+
  geom_line(aes(y= FrecCUM, color= as.character(Infection)), size=1)+
  #xlim(0, 110)+
  #ylim(0, 0.45)+
  scale_fill_manual(values =colorsDis)+
  scale_color_manual(values =colorsDis)+
  theme_bw() +
  theme(text = element_text(size = 30))+
  theme(legend.position = "none")+
  theme(strip.background = element_rect(fill = "white"))+ 
  scale_alpha(guide = 'none')+
      labs(fill= "Rust", x= bquote('Step length' ~ (m^{1/2})), y= "Proportion of Steps")



## ------------------------------------------------------------------------------------
for (nP in unique(DF_TOTAL_AG$numPlants)){
  for (pC in unique(DF_TOTAL_AG$porcionCosecha)){
    
    DF_TOTAL_TEMP <- DF_TOTAL_AG %>%
      filter((numPlants== nP) & (porcionCosecha == pC))
    
    
    DF_TOTAL_TEMP <- DF_TOTAL_TEMP %>%
      group_by(Infection) %>%
      mutate(FrecCUM = cumsum(MeanFrec))
    
    ##aqui un truco para no graficar el que no genera nueva infectin para la escala
    
    DF_TOTAL_TEMP$FrecCUM[DF_TOTAL_TEMP$Infection == "No Infection"] <- NA
    
    
    FIG_HIST_G <- DF_TOTAL_TEMP %>%
      #filter(Infection == "New Infection")%>%
      ggplot(aes(x= DistanceW)) +
      geom_col(aes(y = MeanFrec, fill= as.character(Infection)),
               color= "black", position = "stack")+
      # geom_histogram(color= "black", aes(fill= as.character(Infection), y = after_stat(count / sum(count))),binwidth=1) +
      
      geom_line(aes(y= FrecCUM, group= as.character(Infection)), color= "black",  size=2)+
      geom_line(aes(y= FrecCUM, color= as.character(Infection)), size=1)+
      xlim(0, 10.5)+
      ylim(0, 0.16)+
      scale_fill_manual(values =colorsDis)+
      scale_color_manual(values =colorsDis)+
      theme_bw() +
      theme(text = element_text(size = 30))+
      theme(legend.position = "none")+
      theme(strip.background = element_rect(fill = "white"))+ 
      scale_alpha(guide = 'none')+
      labs(fill= "Rust", x= bquote('Step length' ~ (m^{1/2})), y= "Proportion of Steps")
    
    

  
  #  ggsave(FIG_HIST_G,filename=paste("../../output/graficas/PATH/", "DisHIST_", "nP_", nP, "pC_", pC, ".png", sep=""),  height = 8, width = 14) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
    ggsave(FIG_HIST_G,filename=paste("../../output/graficas/PATH/", "DisHIST_", "nP_", nP, "pC_", pC, ".png", sep=""),  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
    
  }
}




## ------------------------------------------------------------------------------------

DF_MAX_INF <- DF_TOTAL_AG %>%
  filter(Infection == "New Infection")%>%
  group_by(numPlants, Infection, porcionCosecha) %>%
  summarise(MaxFrec = max(FrecCUM))
  


## ------------------------------------------------------------------------------------
DF_SUPER_AG <- DF_TOTAL %>%
  group_by(numPlants, numWorkers,  Infection, porcionCosecha, Rep) %>%
  summarise(FrecAbs =sum(Conteo))

DF_SUPER_AG$Frec <- 2* DF_SUPER_AG$FrecAbs/(DF_SUPER_AG$numPlants)
#el 2 es porque realmente es la mitad del numbero de plantas


DF_SUPER_AG <- DF_SUPER_AG%>%
  group_by(numPlants, numWorkers,  Infection, porcionCosecha) %>%
  summarise(MeanFrec =mean(Frec), sdFrec =sd(Frec))



## ------------------------------------------------------------------------------------
FIG_W_DEN <- DF_SUPER_AG%>%
  filter(Infection == "New Infection")%>%
  ggplot(aes(x = numPlants, y = MeanFrec))+
  geom_line(aes(group = porcionCosecha))+
  
  geom_point(aes(fill= as.character(porcionCosecha), shape= as.character(porcionCosecha)), color= "black", size=5, stroke=1, alpha= 1)+
  #geom_point(aes(shape= as.character(porcionCosecha), color= as.character(porcionCosecha)), size= 3)+
  scale_fill_manual(values = groupColors2)+
  scale_shape_manual(values= c(21, 22, 23))+
  theme_bw() +
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+
  labs(x= "Density (Plants/ha)", y= "Proportion of infective steps", fill= "Harvesting Scenario",shape= "Harvesting Scenario")

ggsave(FIG_W_DEN,filename=paste("../../output/graficas/SUP_FIG/", "FIG_W_DEN.png", sep=""),  height = 6, width = 12)



## ------------------------------------------------------------------------------------
DF_ARRANGED <- DF_TOTAL %>%
  select(HarvestStep, DistanceW, Rep, numPlants, HarvestModel, porcionCosecha, WorkerID, Infection, numWorkers)%>%
  dplyr::mutate(pasoLargo =   round(as.integer(DistanceW/25)/(DistanceW/25))) #truco para que todo valga 1 si es mayor a 18 reportado así como salto laog

DF_CUM <- DF_ARRANGED %>%
  group_by(Rep, numPlants, HarvestModel, porcionCosecha)%>%
  arrange(numPlants, HarvestModel, porcionCosecha, Rep, HarvestStep, WorkerID, numWorkers) %>% 
  dplyr::mutate(cs = cumsum(DistanceW))%>% 
  dplyr::mutate(cumPasoLargo = cumsum(pasoLargo))


#DF_CUM$PerStep <- 100*DF_CUM$HarvestStep/250

#
DF_CUM_A <- DF_CUM %>%
  filter(porcionCosecha == "Asynchronous") %>%
  mutate(PerStep = 100*HarvestStep/250)%>%
  mutate(PerArbBerry = 100*(numPlants/2 - (HarvestStep*numWorkers))/(numPlants))

DF_CUM_SI <- DF_CUM%>%
  filter(porcionCosecha == "Synchronous  Initial")%>%
  mutate(PerStep = 100*HarvestStep/250)%>%
  mutate(PerArbBerry = 100*(numPlants - (HarvestStep*numWorkers))/(numPlants))

DF_CUM_SF <- DF_CUM%>%
  filter(porcionCosecha == "Synchronous Final")%>%
  mutate(PerStep = 100*HarvestStep/250-100)%>%
  mutate(PerArbBerry = 100*(numPlants - (HarvestStep*numWorkers))/(numPlants))


DF_CUM <- rbind(DF_CUM_A, DF_CUM_SI, DF_CUM_SF)


rm(DF_CUM_A)
rm(DF_CUM_SI)
rm(DF_CUM_SF)

#DF_CUM$ArbNoCos <- DF_CUM$numPlants - DF_CUM$HarvestStep

#DF_CUM$PerCos <- 100 -(DF_CUM$ArbNoCos/DF_CUM$numPlants *100)


FIG_CUM <- DF_CUM %>%
  #filter(Rep ==2)%>%
  #filter(WorkerID == "W_2")%>%
  #filter(numPlants== 2000)%>%
  ggplot(aes(x= PerArbBerry, y = DistanceW)) +
  geom_line(aes(color= as.factor(Rep), group= WorkerID))+
  facet_grid(porcionCosecha ~numPlants) +
  theme_bw() +
  scale_color_viridis_d()+
    scale_x_reverse()+
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
  ggplot(aes(x= PerArbBerry, y = DistanceW)) +
  geom_line(aes(group = WorkerID, color = Infection))+
  facet_wrap(~porcionCosecha, nrow = 3, scales = "free_x") +
  #facet_wrap(~porcionCosecha, nrow = 3) +

  theme_bw() +
  scale_color_manual(values = colorsDis2)+
  #geom_segment(aes(x=1.5, y=0, xend= 1.5, yend= 0.025, size = 0.2, color= "Black")+
  theme(text = element_text(size = 25))+
  scale_x_reverse()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  labs(x= "% Trees with berries", y= "Step length per worker (m)", color= "Rust")


ggsave(FIG_CUM_2000,filename=paste("../../output/graficas/PATH/", "FIG_PROGRESSION_2000.png", sep=""),  height = 12, width = 8.5 )



## ------------------------------------------------------------------------------------

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



## ------------------------------------------------------------------------------------
DF_AV <- read.csv("../../data/DF_spatialAverage_complete.csv", header = TRUE)

#we change control model to "no har"

DF_AV$HarvestTime[DF_AV$HarvestModel =="control"] <- "control"
DF_AV$porcionCosecha[DF_AV$HarvestModel =="control"] <- "control"
#DF_AV$numWorkers[DF_AV$HarvestModel =="control"] <- "NoH" 


DF_AV$porcionCosecha[DF_AV$porcionCosecha =="A"] <- " Asynchronous"
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="S_F"] <- " Synchronous Final" 
DF_AV$porcionCosecha[DF_AV$porcionCosecha =="S_I"] <- " Synchronous  Initial" 


DF_AV$HarvestModel[DF_AV$HarvestModel =="closeness"] <- "harvest"



DF_AV$Rust <- DF_AV$Rust*100 #lo pasamos a un porcentage de infeccion para tener una intepretacion as sencilla 


#Our first verification is the number of lines for DF_AV
# we need to have a data frame with 
# [(3 porcCosecha (A, SI, SF)* 1 harvestingTime * 1 numWorkers) + (1 control)]* 5 numPlants * 25 (time max*2+1) times * 30 rep
#[3 * 1 * 1 +1) * 5* 25* 30 rep
# 4 * 5 * 25 * 30
# 15000  rows


## ------------------------------------------------------------------------------------
#here we summarize or average between all repetition- So
#DF_AV_MOD_AVR should be 15000/30 = 500 (combinations)

DF_AV_AVR <- DF_AV %>%
  group_by(numPlants, numWorkers, HarvestModel, HarvestTime,porcionCosecha, Time, SimID) %>%
  summarise(AverageRust = mean(Rust), SD_Rust = sd(Rust))



## ------------------------------------------------------------------------------------
############FIRST FIGURES, time seires, final time, control paarte
#these figures are only plotted without the number of workers that did not show a relevant difference
# so we are plotting only the last time == 12

timeMAX <- 12


FIG_RUST <- DF_AV%>%
  filter(Time == timeMAX)%>%
  filter(numWorkers !=5)%>%
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
  ylim(0,100)+
  labs(x= "Density (Plants/ha)", y= "Average Rust", fill= "Coffee Maturation")

ggsave(FIG_RUST,filename=paste("../../output/graficas/SUP_FIG/", "rust_plants_abs", ".png", sep=""),  height = 10, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).

#


## ------------------------------------------------------------------------------------
FIG_SLI_time <- DF_AV_AVR %>% 
  filter(numWorkers !=5)%>% 
  filter(HarvestTime ==5|HarvestTime =="control")%>% 
  filter(numPlants == 500 | numPlants == 2000 |numPlants == 5000)%>% 
  
  mutate(porcionCosecha = (ifelse(porcionCosecha == "control", "No harvesting (control)", porcionCosecha)))%>% 
  ggplot(aes(x= Time, color= as.character(porcionCosecha) ,
             fill= as.character(porcionCosecha)
             ))+
  geom_line(size= 1, aes(y= AverageRust))+
  geom_ribbon(color= "black", aes( 
                  ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust, linetype= as.character(porcionCosecha)),alpha=0.8) +
  ggtitle("")+
  facet_wrap(~ numPlants, nrow =1)+
  theme(panel.spacing = unit(1, "cm"))+
  scale_linetype_manual(values= c(5,2,1, 0))+
  scale_fill_manual(values = groupColors4)+
  scale_color_manual(values = groupColors4)+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(strip.background = element_rect(fill = "white"))+
  theme(panel.spacing = unit(1, "cm"))+
  labs(x= "Time step (t)", y= "Average Rust (%)", color= "Harvesting scenario", fill= "Harvesting scenario", linetype="Harvesting scenario" )
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SUP_FIG/", "rust_time_abs", ".png", sep=""),  height = 5, width = 16) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc)



## ------------------------------------------------------------------------------------

densidades = length(unique(DF_AV$numPlants))

#this takes the Average and first remove all control rows. and put only time =10
# [(2 porcCosecha (A and S)* 5 harvestingTime * 2 numWorkers)]* 5 numPlants * * 30 rep
#[20] * 5 * 30
# 3000 rows 
# and then it groups by rep (30)*num plants(5) * num Wo (2)* Harvest tine (5) 1500, 
# so we do not include the harvest model in the gripuing, but we put the differeces between the models
# 1500 rows with a new variabe



DF_DIF <- DF_AV %>%
  filter(HarvestModel != "control")%>%  #we remove the contorl, we do not need it.
  filter(Time == timeMAX)

DF_AvsSF <- DF_DIF %>%
  filter(porcionCosecha != " Synchronous  Initial")
DF_AvsSF$Rust[DF_AvsSF$porcionCosecha == " Synchronous Final"] <- DF_AvsSF$Rust[DF_AvsSF$porcionCosecha == " Synchronous Final"]*-1
DF_AvsSF <- DF_AvsSF %>%
  group_by(Rep, numPlants) %>%
  summarise(PerIncrease = sum(Rust))
DF_AvsSF$DIF <- "A-SF"


DF_AvsSI <- DF_DIF %>%
  filter(porcionCosecha != " Synchronous Final")
DF_AvsSI$Rust[DF_AvsSI$porcionCosecha == " Synchronous  Initial"] <- DF_AvsSI$Rust[DF_AvsSI$porcionCosecha == " Synchronous  Initial"]*-1
DF_AvsSI <- DF_AvsSI %>%
  group_by(Rep, numPlants) %>%
  summarise(PerIncrease = sum(Rust))
DF_AvsSI$DIF <- "A-SI"


DF_SFvsSI <- DF_DIF %>%
  filter(porcionCosecha != " Asynchronous")
DF_SFvsSI$Rust[DF_SFvsSI$porcionCosecha == " Synchronous  Initial"] <- DF_SFvsSI$Rust[DF_SFvsSI$porcionCosecha == " Synchronous  Initial"]*-1
DF_SFvsSI <- DF_SFvsSI %>%
  group_by(Rep, numPlants) %>%
  summarise(PerIncrease = sum(Rust))
DF_SFvsSI$DIF <- "SF-SI"

DF_MODELS <- rbind(DF_AvsSI, DF_AvsSF, DF_SFvsSI)
rm(DF_AvsSF, DF_AvsSI, DF_SFvsSI)  



## ------------------------------------------------------------------------------------
FIG_DIF_MODELS <- DF_MODELS%>%
  ggplot(aes(x= as.factor(numPlants), y= PerIncrease))+
  geom_boxplot(color= "black", aes(fill= DIF))+
  ggtitle("")+
  #facet_wrap(~numPlants, nrow=2)+
  scale_fill_manual(values =  c("white", "#CCCCCC", "#666666"))+
  geom_segment(aes(x=0, y=0, xend= 6, yend=0), size = 0.2, color= "DarkRed")+
  theme_bw() +
  #theme(legend.position = "none")+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+
  labs(x= "Density (Plants/ha)", y= "Average Rust Difference")

ggsave(FIG_DIF_MODELS,filename="../../output/graficas/DIF_RUST/dif_models.png",  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



## ------------------------------------------------------------------------------------
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



## ------------------------------------------------------------------------------------
DF_MODvsCON_GEN <- DF_MODvsCON_GEN%>%
filter(porcionCosecha != "control")



## ------------------------------------------------------------------------------------
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
  scale_linetype_manual(values= c(5,2,1, 0))+
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



## ------------------------------------------------------------------------------------
library(knitr)
purl("../../src/grapherCodes/figuresLattice.Rmd", output= "../../src/grapherCodes/figuresLattice.R")


