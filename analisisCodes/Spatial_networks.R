

#this codes makes the analysis for the netwrks

library(ggplot2)
library(dplyr)
library(tidyverse)
library(igraph)
library(reshape2)

mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")
groupColors2 <- c("#1b4a64", "#fd9706", "#fbdb30", "white")#ESTE ES CEMTAL
colorRedes <- c("black", "black","darkred")



#take the dataframe

#DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
rerunAna <- 0

if (rerunAna == 1){

DF_TOTAL <- read.csv("../../data/DF_total_TF.csv", header = TRUE)

DF_TOTAL <- DF_TOTAL %>% 
  filter(porcionCosecha != "S_F")


#remove unuses colunmns
DF_TOTAL$X.1 <- NULL
DF_TOTAL$FruitLoad <- NULL
DF_TOTAL$TotalHarvest <- NULL

 
DF_NEW<-  DF_TOTAL%>%
  group_by(X, Y, Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(NUEVA_RUST = (Rust - Rust[HarvestModel=="control"]), 
            porcionCosecha = porcionCosecha
            )%>%
  filter(porcionCosecha != 1)%>% #para quitar las lineas del control
  filter(NUEVA_RUST ==1)


#esto sirve para correr pruebas puntuales

#nP =2000
#rP = 0
#pC = "A"


#columns = c("Rep", "numPlants",  "porcionCosecha",  "N_REDES",  "T_REDES" , "MAX_T_REDES") 
columns = c("Rep", "numPlants",  "porcionCosecha") 

DF_POST = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(DF_POST) = columns

  for (nP in unique(DF_NEW$numPlants)){
    for (pC in unique(DF_NEW$porcionCosecha)) {
      #contador <- 0
      for (rP in unique(DF_NEW$Rep)) {
      print(c(nP, rP, pC))
      DF_TEMP <- DF_NEW %>%
      filter(numPlants ==nP & Rep == rP & porcionCosecha== pC)
        if (nrow(DF_TEMP)==0){  #esto porque hay comb que no exiten en las nuevas redes
    }
        else
    {  
          N <- dim(DF_TEMP)[1]
  
          distance_matrix <- matrix(0,nrow=N,ncol=N)
          for( i in 1:N){
            for( j in 1:N ){
              distance_matrix[i,j] <- sqrt( (DF_TEMP$X[i] - DF_TEMP$X[j])^2 + (DF_TEMP$Y[i] - DF_TEMP$Y[j])^2 )
        #image(distance_matrix)
        }
    }
    
          Crit_Dist <- 1.5
    
    #MAKE ADJ MATRIX WITH CRITICAL DISTANCE
          adj_matrix <- matrix(0,nrow=N,ncol=N)
    
          for(i in 1:N){
            for(j in 1:N){
              if(i != j & distance_matrix[i,j] <= Crit_Dist){
                adj_matrix[i,j] = 1
                }else{
                  adj_matrix[i,j] = 0
        }
      }
    }
   # image(adj_matrix)
    plot_graph <- graph.adjacency(adj_matrix,mode="undirected")
    #Pull out cluster IDs and put them in the data set
    
    zippin <-layout.fruchterman.reingold(plot_graph)
    zippin[,1] <- DF_TEMP$X
    zippin[,2] <- DF_TEMP$Y
    
    groups <- unlist(clusters(plot_graph)[1])
    DF_TEMP$cluster <- groups
    #Add degree to our data set too
    
    
    
    DF_TEMP$CONTEO <- 1
    #aqui saque el grado a mano
    DF_TEMP <- DF_TEMP %>%
      group_by(Rep, numPlants, NUEVA_RUST, porcionCosecha, cluster)%>%
      summarise(degree = sum(CONTEO)) 
    
    #DF_TEMP$degree <- degree(plot_graph)
    #Assign a color to each group
    numeroRedes <- max(DF_TEMP$cluster) #esto el numero de nuevos 
    
    tamanioRedes <- mean(DF_TEMP$degree)
    maxTamRedes <- max(DF_TEMP$degree)
    #tamanioRedes <- mean(degree(plot_graph))  #esto saca la media ponderada  
    #maxTamRedes <- max(degree(plot_graph))
    
    
    DF_RES <- DF_TEMP %>%
      select(Rep, numPlants, porcionCosecha)
    
    DF_RES <-  head(DF_RES, 1)

    
    DF_RES$N_REDES <- (numeroRedes/DF_RES$numPlants)
    DF_RES$T_REDES <- tamanioRedes
    DF_RES$MAX_T_REDES <-  maxTamRedes
    
    DF_RES$MUL_N_T <- (DF_RES$N_REDES*DF_RES$T_REDES)
    
    
    DF_POST <- rbind(DF_POST, DF_RES)
    
    }
  }
    }
}




#DF_POST <- DF_POST %>%
#  group_by(Rep, numPlants, porcionCosecha)%>%
#  summarise_all(mean)

#falta afinar este resultado!! en realidad el punto es que por cada repeticin haga la multi y desues sacamos des estandarss

DF_POST_RES <- DF_POST %>%
  group_by(numPlants, porcionCosecha)%>%
  select(numPlants, porcionCosecha, N_REDES, T_REDES, MUL_N_T)%>%
  summarise_all(list(mean = ~mean(.), sd = ~sd(.)))


#DF_POST_RES$N_REDES_NOR <- DF_POST_RES$N_REDES/DF_POST_RES$numPlants*100 

DF_POST_RES$porcionCosecha[DF_POST_RES$porcionCosecha =="A"] <- "Asynchronous"
#DF_POST_RES$porcionCosecha[DF_POST_RES$porcionCosecha =="S_F"] <- "Synchronous Final" 
DF_POST_RES$porcionCosecha[DF_POST_RES$porcionCosecha =="S_I"] <- "Synchronous" 



melt_mean <- DF_POST_RES %>%
  select(numPlants, porcionCosecha, N_REDES_mean, T_REDES_mean, MUL_N_T_mean)%>%
  melt(id = c("numPlants","porcionCosecha"), value.name = "value_mean", variable.name = "variable_mean") 

melt_sd <- DF_POST_RES %>%
  select(numPlants, porcionCosecha, N_REDES_sd, T_REDES_sd, MUL_N_T_sd)%>%
  melt(id = c("numPlants","porcionCosecha"), value.name = "value_sd", variable.name = "variable_sd") 


melt_DF_POST_RES <- cbind(melt_mean, melt_sd[,c(3:4)])

melt_DF_POST_RES$comPuesta <- "no" 
melt_DF_POST_RES$comPuesta[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"] <- "si"




write.csv(melt_DF_POST_RES, "../../data/baseDatosREDES_N.csv")

}

#melt_DF_POST_RES <- read.csv("archivosTrabajandose/toyModelHarvest/data/baseDatosREDES_N.csv", header = TRUE)

melt_DF_POST_RES <- read.csv("../../data/baseDatosREDES_N.csv", header = TRUE)


#cambiar antes a caracter
melt_DF_POST_RES$variable_mean  <- as.character(melt_DF_POST_RES$variable_mean)


melt_DF_POST_RES$variable_mean[melt_DF_POST_RES$variable_mean == "N_REDES_mean"] <- "i. Infected networks AH/density"
melt_DF_POST_RES$variable_mean[melt_DF_POST_RES$variable_mean == "T_REDES_mean"] <- "ii. Plants/infected network AH"
melt_DF_POST_RES$variable_mean[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"] <- "iii. Percentage of infected plants AH/density (i x ii*100)"

melt_DF_POST_RES$value_mean[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"] <- melt_DF_POST_RES$value_mean[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"]*100
melt_DF_POST_RES$value_sd[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"] <- melt_DF_POST_RES$value_sd[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"]*100


FIG_REDES <- melt_DF_POST_RES %>% 
  #  filter(comPuesta =="no")%>% 
  ggplot(aes(x= numPlants, y= value_mean))+
  geom_line(size=1, aes(linetype = porcionCosecha, group=porcionCosecha), color= "black")+
  geom_errorbar(size=0.5, aes( 
    ymin=value_mean-value_sd, ymax=value_mean+value_sd), color= "black") +
  geom_point(size= 6, aes(fill = porcionCosecha, shape= porcionCosecha), color="black", stroke= 1)+
  scale_fill_manual(values = groupColors2)+
  scale_shape_manual(values = c(21, 24, 22))+
  #scale_color_manual(values = colorRedes)+
  scale_linetype_manual(values = c(1,1,2))+
  facet_wrap(~variable_mean, scales = "free_y")+
  theme_bw()+
  theme(text = element_text(size = 25), 
        legend.key.size=unit(1,"cm"))+
  theme(strip.background = element_rect(colour= "black",
                                        fill="white"))+
  #theme(legend.position = "none") +
  labs(x ="Density (Plants/ha)", y="Magnitude", shape= "Legend", linetype= "Legend", fill= "Legend")


ggsave(FIG_REDES,filename=paste("../../output/graficas/NETWORKS/", "redesAna2", ".pdf", sep=""),  height = 8, width = 26) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).
ggsave(FIG_REDES,filename=paste("../../output/graficas/NETWORKS/", "redesAna2", ".png", sep=""),  height = 8, width = 26) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).



# FIG_REDES_VIEJA <- melt_DF_POST_RES %>% 
# #  filter(comPuesta =="no")%>% 
#   ggplot(aes(x= numPlants, y= value_mean))+
#   geom_line(size=1, aes(color = variable_mean, linetype = variable_mean, group=variable_mean))+
#   geom_errorbar(size=0.5, aes( 
#     ymin=value_mean-value_sd, ymax=value_mean+value_sd, color = variable_mean)) +
#   geom_point(size= 6, aes(color = variable_mean, shape= variable_mean), fill="white", stroke= 1)+
#   scale_shape_manual(values = c(21, 24, 22))+
#   scale_color_manual(values = colorRedes)+
#   scale_linetype_manual(values = c(1,1,2))+
#   facet_wrap(~porcionCosecha* variable_mean, scales = "free_y")+
#   theme_bw()+
#   theme(text = element_text(size = 25), 
#         legend.key.size=unit(1,"cm"))+
#   theme(strip.background = element_rect(colour= "black",
#                                         fill="white"))+
#   #theme(legend.position = "none") +
#   labs(x ="Density (Plants/ha)", y="Magnitude", shape= "Legend", linetype= "Legend", color= "Legend")






