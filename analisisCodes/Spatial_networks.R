

library(ggplot2)
library(dplyr)
library(tidyverse)
mycols <- c("#021128","#1b4a64", "#3585a0", "#759580", "#c78f34", "#fd9706","#fdb81c","#fbdb30")
mycols3a <-c("#021128", "#fd9706", "#1b4a64", "#759580")
mycols3b <-c("#1b4a64", "#fdb81c", "#759580")
mycols3c <- c("#759580", "#1b4a64","#fdb81c")

DF_TOTAL <- read.csv("archivosTrabajandose/toyModelHarvest/data/DF_total_TF.csv", header = TRUE)

DF_TOTAL$X.1 <- NULL
DF_TOTAL$FruitLoad <- NULL
DF_TOTAL$TotalHarvest <- NULL


DF_TOTAL <- DF_TOTAL %>%
  #filter(Time ==10)%>%
  filter(Rust ==1)

DF_NEW<-  DF_TOTAL%>%
    group_by(Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
    summarise(NUEVA_RUST = ((Rust - Rust[HarvestModel=="control"])),
              porcionCosecha = porcionCosecha, HarvestTime = HarvestTime, numWorkers = numWorkers, Time= Time)  # esto es solo para que despues de leer por condicion, regrese...?
  
DF_NEW <- DF_NEW %>%
  filter(NUEVA_RUST ==1)  #entonces estas son las nuevas redes por condicion


DF_NET <- data.frame(names(DF_NEW))
DF_NET$N_REDES <- 0
DF_NET$T_REDES <- 0

for (sim in unique(DF_NEW$SimID)){
  for (r in unique(DF_NEW$Rep)) {
    
    DF_TEMP <- DF_NEW %>%
      filter(SimID =sim & Rep = r)
    
    N <- dim(DF_TEMP)[1]
    
    distance_matrix <- matrix(0,nrow=N,ncol=N)
    for( i in 1:N){
      for( j in 1:N ){
        distance_matrix[i,j] <- sqrt( (x_coords[i] - x_coords[j])^2 + (y_coords[i] - y_coords[j])^2 )
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
    
    #Make the network
    plot_graph <- graph.adjacency(adj_matrix,mode="undirected")
    #Pull out cluster IDs and put them in the data set
    groups <- unlist(clusters(plot_graph)[1])
    numeroRedes <- max(groups) #esto el numero de nuevos 
    tamanioRedes <- mean(degree(plot_graph))  #esto saca la media ponderada  
    
    
    DF_TEMP$N_REDES <- numeroRedes
    DF_TEMP$T_REDES <- tamanioRedes
    
    
    DF_NET <- rbind(DF_NET, DF_TEMP)
    
  }
}



















DF_TOTAL_SAMPLE <- DF_TOTAL %>%
  filter(Time ==10)%>%
  filter(Rust ==1)%>%
  filter(SimID == 61 & Rep ==0)

DF_TOTAL_SAMPLE 



image(distance_matrix)



## set the spatial threshold for connecting plants 


## this is the matrix representation of the network 
image(adj_matrix)



## Using igraph to work with the 
#install.packages("igraph")
library(igraph)


#Make the network
plot_graph <- graph.adjacency(adj_matrix,mode="undirected")
#Pull out cluster IDs and put them in the data set
groups <- unlist(clusters(plot_graph)[1])
DF_TOTAL_SAMPLE$cluster <- groups
#Add degree to our data set too
DF_TOTAL_SAMPLE$degree <- degree(plot_graph)
#Assign a color to each group
V(plot_graph)$color <- DF_TOTAL_SAMPLE$cluster

## Take some layout and then change the coordinates to match our real xy positions
zippin <-layout.fruchterman.reingold(plot_graph)
zippin[,1] <- x_coords
zippin[,2] <- y_coords



plot(plot_graph,layout = zippin,vertex.size = 1, vertex.label = NA,edge.arrow.size = 0.1,edge.color="red",edge.curved=0,vertex.color="black",edge.width=2,frame=T)




## checking to make sure that the degrees are calculate dcorrectly 
plot(df$x_coord,df$y_coord,cex=0)
text(df$x_coord,df$y_coord,rownames(df),cex=0.6)
df

plot(df$x_coord,df$y_coord,cex=2,col=df$cluster,pch=19)


df[df$id == 6, ]
df[df$id == 17, ]


df









