
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)
library(reshape2)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#3585a0")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")


LATTICE_DF <- read.csv("../data/intentoDF.csv")



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
   # theme(legend.position="none")
  
  ggsave(FIG_RUST,filename=paste("../output/figurasLattice/",n,"contactModel",t,".png",sep="")) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
  n= n+1
  # jpg(file=mypath)
  # mytitle = paste("contactModel", i)
  # plot(x,y, main = mytitle)
  # dev.off()
}


# FIG_RUST <- LATTICE_DF %>%
#   ggplot()+
#   geom_point(aes(x=X , y= Y, color= as.character(Rust)), size= 2)+
#   ggtitle("")+
#   scale_color_manual(values = mycols3c)+
#   facet_wrap( ~ Time, nrow=2)+
#   theme_bw()+
#   theme(legend.position="none")
# 
# ggsave("../output/figuraLattice.png", path= "../output/", width= 8, height = 8)
