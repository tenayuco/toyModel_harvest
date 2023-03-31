
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")


#LATTICE_DF <- read.csv("../data/intentoDF.csv") #este es para correrlo desde la termunal
LATTICE_DF <- read.csv("archivosTrabajandose/toyModelHarvest/data/intentoDF.csv", header = TRUE)

LATTICE_DF$Total <- 1

N_PLANTS <- max(LATTICE_DF$ID)+1

LATTICE_RES <- LATTICE_DF %>%
  group_by(Jump, Rep, Time, Rust)%>%
  summarise(Total_Sum = sum(Total)/N_PLANTS)



n= 0
for(t in unique(LATTICE_DF$Time)){
  FIG_RUST <- LATTICE_DF %>%
    filter(Time == t) %>%
    add_row(Jump = 0,  Rep= 0,   ID= 999999,  X= NA,  Y=NA , Harvest = 0, Rust= 0)%>% ##ESte es un truquito para que siempre hayan 3 colores en cada graficas, porque le 0.5 siempre desaprece
    add_row(Jump = 0,  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Harvest = 0, Rust= 0.5)%>%
    add_row(Jump = 0,  Rep= 0,   ID= 9999999,  X= NA,  Y=NA , Harvest = 0, Rust= 1)%>%
    ggplot()+
    geom_point(aes(x=X , y= Y, color= as.character(Rust)), size= 3)+
    ggtitle("")+
    scale_color_manual(values = mycols3c)+
    theme_bw()
  
  ggsave(FIG_RUST,filename=paste("../output/figurasLattice/",n,"contactModel",t,".png",sep="")) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  n= n+1
}


FIG_SLI_time <- LATTICE_RES %>%
  ggplot()+
  geom_line(aes(x=Time , y= Total_Sum, group= as.character(Rust)), color= "black")+ 
  geom_point(aes(x=Time , y= Total_Sum, color= as.character(Rust)), size= 2)+
  ggtitle("")+
  scale_color_manual(values = mycols3c)+
  theme_bw()

FIG_PATH<- LATTICE_DF %>% 
  filter(Time == 5)%>% 
  filter(HarvestEvent !=0)%>% 
  rowwise() %>% 
  ggplot(aes(x= X, y = Y)) +
  geom_path(aes(col= WorkerID),size=1)+
  geom_point(size=1)+ # es importante que sea path, porque así lo hace según coo estan ordenados los
 # scale_color_viridis_c()+
  facet_wrap(~HarvestEvent, ncol=2)+
  theme(panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  theme_bw()+
  labs(x= "X_norm", y= "Y_norm", col= "Minutes")

ggsave(FIG_SLI_time,filename="../output/graficas/SLI_time.png") # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).


