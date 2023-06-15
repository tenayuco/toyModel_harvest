library(ggplot2)
library(dplyr)
library(tidyverse)
library(igraph)
library(reshape2)

mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")
groupColors2 <- c("#fd9706", "#1b4a64",'#56B4E9', "#555555")
colorsDis3 <- c("white","black", "#999999")
#colorsDis3 <- c("#006600","#BA110C", "#777777")
#colorsDis3 <- c("#006600","#BA110C", "#777777")


#take the dataframe

#DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)
DF_TOTAL <- read.csv("../../data/DF_total_TF.csv", header = TRUE)


#remove unuses colunmns
DF_TOTAL$X.1 <- NULL
DF_TOTAL$FruitLoad <- NULL
DF_TOTAL$TotalHarvest <- NULL

#filter conditions that are unused 
DF_TOTAL<- DF_TOTAL %>%
  filter(numWorkers != 5)%>%
  filter(numPlants != 4000)
# filter(Rep ==0) #prueba

##aqui deberiamos tener

#  (1 harTime x  2 porCose  + 1 control) x (500+1000+2000+3000+5000) x N rep 
# 3 * 11500 * 6

DF_TOTAL$porcionCosecha[DF_TOTAL$HarvestModel =="control"] <- 99

###############################



DF_TODAS<-  DF_TOTAL%>%
  group_by(X, Y, Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(NUEVA_RUST = (Rust + Rust[HarvestModel=="control"]), 
            porcionCosecha = (porcionCosecha- porcionCosecha[HarvestModel=="control"]) +99)%>%
  filter(porcionCosecha <98)#para quitar las lineas del control

#aqui queda 0 cuando nunca se infeco, 1 cuando es nueva infeccion, 2 cuando ya estaba infectado


nP =2000
rP = 0
pC = 1


for (nP in unique(DF_TODAS$numPlants)){
  for (pC in unique(DF_TODAS$porcionCosecha)) {
    rP = 0
      print(c(nP, pC, rP))
      DF_TEMP <- DF_TODAS %>%
        filter(numPlants ==nP & Rep == rP & porcionCosecha== pC)
      
      if (nrow(DF_TEMP)==0){
        rP = rP+1
        DF_TEMP <- DF_TODAS %>%
          filter(numPlants ==nP & Rep == rP & porcionCosecha== pC)#esto porque hay comb que no exiten en las nuevas redes
      }
      
      #aqui hacemos un truco unico para la visualizacion, corto 10 m
      
      DF_TEMP <- DF_TEMP %>%
        filter(X<30)%>%
        filter(Y<30)
    
        
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
        
        
        zippin <-layout.fruchterman.reingold(plot_graph)
        zippin[,1] <- DF_TEMP$X
        zippin[,2] <- DF_TEMP$Y
        
        # plot(plot_graph,layout = zippin,vertex.size = 1, vertex.label = NA,edge.arrow.size = 0.1,edge.color="red",edge.curved=0,vertex.color="black",edge.width=2,frame=T)
        vecColor <- colorsDis3[(DF_TEMP$NUEVA_RUST+1)]
        
        
        
        jpeg(paste("../../output/network_","numPlants_", nP, "coffee maturation_", pC, "rep_", rP, ".jpg"), width = 10, height = 10, unit="in", res=300)
        par(mar = c(0.1, 0.1, 0.1, 0.1))
        plot(plot_graph,layout = zippin,vertex.size = 7, vertex.label = NA,edge.arrow.size = 0.1,
               edge.color="black",edge.curved=0,vertex.color= vecColor ,edge.width=3,frame=T)
          dev.off()
        

        
        #main = paste("numPlants", nP, "coffee maturation", pC, "rep", rP)  
        
      
    }
  }

