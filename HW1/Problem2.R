library(igraph)

#test the propotion of connected graph
count<-0
dia<-0
for(i in 1:100){
  fat_tailed_graph <- barabasi.game(1000,power=-3,directed=FALSE)
  dia<-dia+diameter(fat_tailed_graph)
  if(is.connected(fat_tailed_graph))
  {
  count<-count+1
  }
}
dia<-dia/100
k<-count/100

#create a undirected network with a fat-tailed degree distribution
fat_tailed_graph <- barabasi.game(1000,power=-3,directed=FALSE)
degreesVector = degree(fat_tailed_graph)
h <- hist(degreesVector, freq=FALSE, seq(-0.5,by=1,length.out=max(degreesVector)+2), xlab="degree", ylab="density", main="Degree Distribution")


# find the giant connected component
cl <- clusters(fat_tailed_graph)
gccIndex = which.max(cl$csize)
nonGccNodes <- (1:vcount(fat_tailed_graph))[cl$membership != gccIndex]
gcc <- delete.vertices(fat_tailed_graph, nonGccNodes)

# find communities using fastgreedy algorithm
fg_com <- fastgreedy.community(gcc)
modularity(fg_com)

##create a undirected network with a fat-tailed degree distribution with 10000 nodes
fat_tailed_graph2 <- barabasi.game(10000,power=-3,directed = FALSE)
fg_com2 <- fastgreedy.community(fat_tailed_graph2)
modularity(fg_com2)

degreesVector <- degree(fat_tailed_graph2)
for (p in 1:10000){
  #randomly choose node i
  i<-sample(1:10000,1)
  b<-sample(1:degreesVector[i],1)
  #randomly choose the neighborhood node of i
  j<-neighborhood(fat_tailed_graph2,1,i)[[1]][b+1]
  if(p==1){
    deg<-degreesVector[j]
    }else{
      deg<-c(deg,degreesVector[j])
    }
}
hs <- hist(deg, breaks=seq(-0.5, by=1, length.out=max(deg)+2), xlab="degree", ylab="node number", main="Degree Distribution of j")


