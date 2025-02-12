### esto es solo una copia del codigo de otro aarticulo para sacar lsod dos jemplos

library(patchwork)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(moveHMM)
library(knitr)
library(reshape2)
colorsGris <- c("black","#555555", "white")

mycols3c <-c("#759580", "#1b4a64")


#these 2 colors contrast weel for color blind people


#DF_HARVEST_GAMMA <- read.csv("/home/emilio/archivosTrabajandose/toyModelHarvest/data/datosParaFiguras.csv")
DF_HARVEST_GAMMA <- read.csv("../../data/datosParaFiguras.csv")


#aqui invierto el 1 con el 2 por medio de una funcon linea

DF_HARVEST_GAMMA$state <-  (-DF_HARVEST_GAMMA$state) + 3

## Y agrego el nombre (long y short step)
#ver bien cuál quiero

#DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "1"] <- "1 (short steps)"
#DF_HARVEST_GAMMA$state[DF_HARVEST_GAMMA$state == "2"] <- "2 (long steps)"



DF_HARVEST_GAMMA <- DF_HARVEST_GAMMA %>%
  separate(ID, into = c("Finca", "IDREC"), sep = "_", remove = T)


DF_HARVEST_GAMMA_H <- DF_HARVEST_GAMMA%>%
  filter(Finca == "H")%>%
  group_by(IDREC) %>%
  mutate(ID_POR_FINCA = cur_group_id())

DF_HARVEST_GAMMA_I <- DF_HARVEST_GAMMA %>%
  filter(Finca == "I")%>%
  group_by(IDREC) %>%
  mutate(ID_POR_FINCA = cur_group_id())

DF_HARVEST_GAMMA <- rbind(DF_HARVEST_GAMMA_I, DF_HARVEST_GAMMA_H)  

rm(DF_HARVEST_GAMMA_I)
rm(DF_HARVEST_GAMMA_H)


#ahora estan umberados del 1 al 6 dentro de cada finca
#codesnames

#cambiams por E de ecolo
DF_HARVEST_GAMMA$Finca[DF_HARVEST_GAMMA$Finca== "H"] <- "C"
DF_HARVEST_GAMMA$Finca[DF_HARVEST_GAMMA$Finca== "I"] <- "A"



DF_HARVEST_GAMMA_2<- DF_HARVEST_GAMMA %>% 
  unite("Finca_ID", c(Finca, ID_POR_FINCA), remove = F) %>% 
  filter(Finca_ID %in% c("C_4", "C_1", "A_3"))

DF_HARVEST_GAMMA_2$Finca_ID[DF_HARVEST_GAMMA_2$Finca_ID== "C_4"] <- "C_ 4"

DF_HARVEST_RESUMEN <- DF_HARVEST_GAMMA_2 %>%
  group_by(Finca, ID_POR_FINCA)%>%
  summarise(observation = sum(conteo)) %>%
  unite("Finca_ID", c(Finca, ID_POR_FINCA))

DF_HARVEST_RESUMEN$Finca_ID[DF_HARVEST_RESUMEN$Finca_ID== "C_4"] <- "C_ 4"







FIG_MAP_GAMMA_EX <- DF_HARVEST_GAMMA_2 %>% 
  ggplot(aes(x= x, y = y)) +
  geom_path(aes(col= as.factor(Finca), group = (Finca_ID)), size= 1.5)+
  geom_point(size=1.5, aes(fill= "Tree"))+ # es importante que sea path, porque así lo hace según coo estan ordenados los
  scale_color_manual(values= mycols3c)+
  xlim(0,100)+
  ylim(0,130)+
  facet_wrap(~Finca_ID, ncol = 1)+
  theme_bw()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(legend.position = "None")+
  theme(text = element_text(size = 20))+
  geom_text(x = 85, y = 80, aes(label = observation), data = DF_HARVEST_RESUMEN, size= 5)+
  geom_text(x = 70, y = 80, label= "N =", size= 5)+
  theme(text = element_text(size = 20))+
  #theme(strip.background =element_rect(fill="white"))+
  #theme(strip.background = element_blank(), panel.spacing = unit(0.8, "lines"), text = element_text(size = 15))+
  labs(x= "X (m)", y= "Y (m)", col= "Plantation", fill= "")


#ggsave(FIG_MAP_GAMMA_EX, filename= "/home/emilio/archivosTrabajandose/toyModelHarvest/output/graficas/mapHarvest_ex.png", height = 12, width = 5, device = "png")

ggsave(FIG_MAP_GAMMA_EX, filename= "../../output/graficas/mapHarvest_examples.png", height = 10, width = 4.5, device = "png")
