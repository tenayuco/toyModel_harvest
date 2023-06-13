

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
groupColors2 <- c("#fd9706", "#1b4a64",'#56B4E9', "#555555")
colorRedes <- c("black", "black", "darkred")



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



DF_NEW<-  DF_TOTAL%>%
  group_by(X, Y, Rep, numPlants)%>% #son las vairables que quedan y sobre esos escenarios, vamos a hacer las diferencias entre modelos
  summarise(NUEVA_RUST = (Rust - Rust[HarvestModel=="control"]), 
            porcionCosecha = (porcionCosecha- porcionCosecha[HarvestModel=="control"]) +99)%>%
  filter(porcionCosecha <98)%>% #para quitar las lineas del control
  filter(NUEVA_RUST ==1)


#nP =5000
#rP = 0
#pC = 1


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
    
   # print("con")
  #  print(contador)
   # if (contador ==0){
  #  plot(plot_graph,layout = zippin,vertex.size = 1, vertex.label = NA,edge.arrow.size = 0.1,
   #      edge.color="red",edge.curved=0,vertex.color="black",edge.width=2,frame=T,
    #     main = paste("numPlants", nP, "coffee maturation", pC, "rep", rP))
   #}
  #  contador <- contador+1
    
    DF_TEMP <- DF_TEMP %>%
      select(Rep, numPlants, porcionCosecha)
    
    DF_TEMP$N_REDES <- (2* numeroRedes/DF_TEMP$numPlants)*10
    DF_TEMP$T_REDES <- tamanioRedes
    DF_TEMP$MAX_T_REDES <-  maxTamRedes
    DF_TEMP$MUL_N_T <- DF_TEMP$N_REDES*DF_TEMP$T_REDES
    
    
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
  select(numPlants, porcionCosecha, N_REDES, T_REDES, MUL_N_T)%>%
  summarise_all(list(mean = ~mean(.), sd = ~sd(.)))


#DF_POST_RES$N_REDES_NOR <- DF_POST_RES$N_REDES/DF_POST_RES$numPlants*100 

DF_POST_RES$porcionCosecha[DF_POST_RES$porcionCosecha =="0.5"] <- "Asynchronous"
DF_POST_RES$porcionCosecha[DF_POST_RES$porcionCosecha =="1"] <- "Synchronous"

melt_mean <- DF_POST_RES %>%
  select(numPlants, porcionCosecha, N_REDES_mean, T_REDES_mean, MUL_N_T_mean)%>%
  melt(id = c("numPlants","porcionCosecha"), value.name = "value_mean", variable.name = "variable_mean") 

melt_sd <- DF_POST_RES %>%
  select(numPlants, porcionCosecha, N_REDES_sd, T_REDES_sd, MUL_N_T_sd)%>%
  melt(id = c("numPlants","porcionCosecha"), value.name = "value_sd", variable.name = "variable_sd") 


melt_DF_POST_RES <- cbind(melt_mean, melt_sd[,c(3:4)])

melt_DF_POST_RES$comPuesta <- "no" 
melt_DF_POST_RES$comPuesta[melt_DF_POST_RES$variable_mean == "MUL_N_T_mean"] <- "si"

FIG_REDES <- melt_DF_POST_RES %>% 
#  filter(comPuesta =="no")%>% 
  ggplot(aes(x= numPlants, y= value_mean))+
  geom_point(size= 1.5, aes(color = variable_mean))+
  geom_line(size=1, aes(color = variable_mean, linetype = variable_mean))+
  geom_errorbar(size=1, aes( 
    ymin=value_mean-value_sd, ymax=value_mean+value_sd, color = variable_mean, linetype = variable_mean)) +
  scale_color_manual(values = colorRedes)+
  facet_wrap(~porcionCosecha, scales = "free_y")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "Normalized number of networks (x10)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Mean network degree")
  ) +
  theme_bw()+
  theme(text = element_text(size = 25))+
  theme(strip.background = element_rect(fill = "white"))
  
ggsave(FIG_REDES,filename=paste("../../output/graficas/", "redesAna", ".png", sep=""),  height = 8, width = 20) # ID will be the unique identifier. and change the extension from .png to whatever you like (eps, pdf etc).










