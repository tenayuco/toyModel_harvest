
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")


LATTICE_DF <- read.csv("../data/intentoDF.csv") #este es para correrlo desde la termunal
#LATTICE_DF <- read.csv("archivosTrabajandose/toyModelHarvest/data/intentoDF.csv", header = TRUE)

LATTICE_DF$Total <- 1

LATTICE_DF$HarvestTime[LATTICE_DF$HarvestModel == "Control"] <- 0 

N_PLANTS <- max(LATTICE_DF$ID)+1


LATTICE_RES_SPA <- LATTICE_DF %>%  #esta nos da un dataframe qyue promedia sobre el espacio
  group_by(HarvestModel, HarvestTime, numWorkers, Rep, Time, Rust)%>%
  summarise(Total_Sum = sum(Total)/N_PLANTS)

LATTICE_HAR <- LATTICE_DF %>% #esta agarra solo la cosecha por trabajador
  filter(Time==5)%>%
  filter(WorkerID !=0)%>%
  filter(HarvestModel == "True")%>%
  group_by(HarvestModel, HarvestTime, numWorkers, Rep, WorkerID)%>%  
  summarise(CosechaTotalWorker = sum(TotalHarvest))

LATTICE_HAR$CosechaTotal <- LATTICE_HAR$CosechaTotalWorker*LATTICE_HAR$numWorkers  #pensar si esto tiene sentido

LATTICE_RES_SPA_AVE <- LATTICE_RES_SPA %>%
  group_by(HarvestModel, HarvestTime, numWorkers, Time, Rust)%>%
  summarise(AverageRust = mean(Total_Sum), SD_Rust = sd(Total_Sum))

###########Plo

FIG_SLI_time <- LATTICE_RES_SPA_AVE %>%  ##FATLA POMERLA EL SD
  filter(Rust == 1) %>%
  ggplot()+
  geom_line(aes(x=Time , y= AverageRust, group= interaction(HarvestTime)), color="black")+ 
  geom_point(aes(x=Time , y= AverageRust, color= interaction(HarvestTime)), size= 2)+
  ggtitle("")+
  facet_wrap(~numWorkers, ncol=2)+
  scale_color_manual(values = mycols)+
  theme_bw()

ggsave(FIG_SLI_time,filename="../output/graficas/SLI_time.png",  height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_HAR_W <- LATTICE_HAR %>%
  ggplot()+
  geom_boxplot(aes(x= as.character(HarvestTime), y= CosechaTotal, color= as.character(numWorkers), group= interaction(numWorkers, HarvestTime)))+
  ggtitle("")+
  #facet_wrap(~numWorkers, ncol=2, scales = "free")+
  scale_color_manual(values = mycols3a)+
  theme_bw()

ggsave(FIG_HAR_W,filename="../output/graficas/HAR_W.png", height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


FIG_RUST <- LATTICE_RES_SPA%>%
  filter(Rust == 1)%>%
  filter(Time == 5)%>%
  ggplot()+
  geom_boxplot(aes(x= as.character(HarvestTime), y= Total_Sum, color= as.character(numWorkers), group= interaction(numWorkers, HarvestTime)))+ 
  ggtitle("")+
  #facet_wrap(~numWorkers, ncol=2, scales = "free")+
  scale_color_manual(values = mycols3a)+
  theme_bw()

ggsave(FIG_RUST,filename="../output/graficas/RUST_W_Htime.png", height = 8, width = 10) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



FIG_PATH<- LATTICE_DF %>% 
  filter(Time == 5)%>% 
  filter(Rep == 0)%>% #solo un ejepmplo
  filter(HarvestTime ==1)%>%  #da igial
  filter(HarvestModel == "True")%>% 
  filter(WorkerID != 0)%>% 
  arrange(WorkerID, HarvestStep)%>%  #importante para que se orden por pasos, y despues se hace por worker!!
  rowwise() %>% 
  ggplot(aes(x= X, y = Y, group=WorkerID)) +
  geom_path(aes(col= WorkerID),linewidth=1)+
  geom_point(size=1)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  #scale_color_viridis_c()+
  scale_color_manual(values = mycols)+
  facet_wrap(~numWorkers, ncol=2)+
  theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  theme_bw()+
  labs(x= "X_norm", y= "Y_norm", col= "Minutes")

ggsave(FIG_PATH,filename="../output/graficas/PATH.png",  height = 8, width = 12) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



wholePlot <- 0

n= 0
if (wholePlot ==1){
for(t in unique(LATTICE_DF$Time)){
  FIG_RUST <- LATTICE_DF %>%
    filter(Rep ==0)%>%
    filter(Time == t) %>%
    add_row(HarvestModel = 0,  Rep= 0,   ID= 999999,  X= NA,  Y=NA , Rust= 0)%>% ##ESte es un truquito para que siempre hayan 3 colores en cada graficas, porque le 0.5 siempre desaprece
    add_row(HarvestModel = 0,  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Rust= 0.5)%>%
    add_row(HarvestModel = 0,  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Rust= 1)%>%
    ggplot()+
    geom_point(aes(x=X , y= Y, color= as.character(Rust)), size= 3)+
    ggtitle("")+
    scale_color_manual(values = mycols3c)+
    theme_bw()
  
  ggsave(FIG_RUST,filename=paste("../output/figurasLattice/",n,"contactModel",t,".png",sep="")) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  n= n+1
}
}



