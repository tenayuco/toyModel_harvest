

library(ggplot2)
library(dplyr)
library(tidyverse)
library(igraph)

mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")



DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)

DF_TOTAL$X.1 <- NULL
DF_TOTAL$FruitLoad <- NULL
DF_TOTAL$TotalHarvest <- NULL

DF_TOTAL<- DF_TOTAL %>%
  filter(numWorkers != 5)%>%
  filter(numPlants != 4000)
 # filter(Rep ==0) #prueba

##aqui deberiamos tener

#  (1 harTime x  2 porCose  + 1 control) x (500+1000+2000+3000+5000) x N rep 
   # 3 * 11500 * 6

DF_TOTAL$porcionCosecha[DF_TOTAL$HarvestModel =="control"] <- 99


DF_NEW<-  DF_TOTAL%>%
  group_by(X, Y, Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(NUEVA_RUST = (Rust - Rust[HarvestModel=="control"]), 
            porcionCosecha = (porcionCosecha- porcionCosecha[HarvestModel=="control"]) +99)%>%
  filter(porcionCosecha <98)%>% #para quitar las lineas del control
  filter(NUEVA_RUST ==1)


nP =1000
rP = 0
pC = 0.5


columns = c("Rep", "numPlants",  "porcionCosecha",  "N_REDES",  "T_REDES" , "MAX_T_REDES") 
DF_POST = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(DF_POST) = columns


for (nP in unique(DF_NEW$numPlants)){
  for (rP in unique(DF_NEW$Rep)) {
    for (pC in unique(DF_NEW$porcionCosecha)) {
      
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
    groups <- unlist(clusters(plot_graph)[1])
    
    DF_TEMP$cluster <- groups
    #Add degree to our data set too
    DF_TEMP$degree <- degree(plot_graph)
    #Assign a color to each group
    numeroRedes <- max(groups) #esto el numero de nuevos 
    tamanioRedes <- mean(degree(plot_graph))  #esto saca la media ponderada  
    maxTamRedes <- max(degree(plot_graph))
    
    zippin <-layout.fruchterman.reingold(plot_graph)
    zippin[,1] <- DF_TEMP$X
    zippin[,2] <- DF_TEMP$Y
    
    
    
   # plot(plot_graph,layout = zippin,vertex.size = 1, vertex.label = NA,edge.arrow.size = 0.1,edge.color="red",edge.curved=0,vertex.color="black",edge.width=2,frame=T)
    
    DF_TEMP <- DF_TEMP %>%
      select(Rep, numPlants, porcionCosecha)
    
    DF_TEMP$N_REDES <- numeroRedes
    DF_TEMP$T_REDES <- tamanioRedes
    DF_TEMP$MAX_T_REDES <-  maxTamRedes
    
    DF_POST <- rbind(DF_POST, DF_TEMP)
    
    }
  }
}
}

DF_POST <- DF_POST %>%
  group_by(Rep, numPlants, porcionCosecha)%>%
  summarise_all(mean)


#falta afinar este resultado!! en realidad el punto es que por cada repeticin haga la multi y desues sacamos des estandarss

DF_POST_RES <- DF_POST %>%
  group_by(numPlants, porcionCosecha)%>%
  summarise_all(list(mean = ~mean(.), sd = ~sd(.)))


DF_POST_RES$N_REDES_NOR <- DF_POST_RES$N_REDES/DF_POST_RES$numPlants*100 

DF_POST_RES$MUL_N_T <- DF_POST_RES$N_REDES_NOR*DF_POST_RES$T_REDES



FIG_REDES <- DF_POST_RES %>% 
  ggplot(aes(x= numPlants))+
  geom_line(size= 1.5, aes(y= MUL_N_T, color= as.character(porcionCosecha)))+
  geom_ribbon(aes( 
    ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust),alpha=0.5) +

#+




  #geom_point(aes(y= AverageRust, color= as.character(porcionCosecha)), size= 2)+
  geom_ribbon(aes( 
    ymin=AverageRust-SD_Rust, ymax=AverageRust+SD_Rust),alpha=0.5) +
  ggtitle("")+
  facet_wrap(~ numPlants, nrow =1)+
  scale_fill_manual(values = groupColors4)+
  scale_color_manual(values = groupColors4)+
  theme_bw()+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))+ 
  #theme(legend.position = "none")
  labs(x= "Time", y= "Average Rust", color= "Coffee Maturation", fill= "Coffee Maturation")
ggsave(FIG_SLI_time,filename=paste("../../output/graficas/SUP_FIG/", "rust_time_abs", ".png", sep=""),  height = 8, width = 22) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc)













