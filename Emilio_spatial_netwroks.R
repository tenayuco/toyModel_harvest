




N <- 5000

## make soem coordinates 
x_coords <- runif(N,min=0,max=100)
y_coords <- runif(N,min=0,max=100)

df <- data.frame(x_coords,y_coords)

df$id <- seq(1,N,1)

## calculate a distance matrix 
N <- length(x_coords)
distance_matrix <- matrix(0,nrow=N,ncol=N)
for( i in 1:N){
	for( j in 1:N ){
		distance_matrix[i,j] <- sqrt( (x_coords[i] - x_coords[j])^2 + (y_coords[i] - y_coords[j])^2 )
	}
}


image(distance_matrix)



## set the spatial threshold for connecting plants 
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

## this is the matrix representation of the network 
image(adj_matrix)



## Using igraph to work with the 
#install.packages("igraph")
library(igraph)


#Make the network
plot_graph <- graph.adjacency(adj_matrix,mode="undirected")
#Pull out cluster IDs and put them in the data set
groups <- unlist(clusters(plot_graph)[1])
df$cluster <- groups
#Add degree to our data set too
df$degree <- degree(plot_graph)
#Assign a color to each group
V(plot_graph)$color <- df$cluster
df

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









