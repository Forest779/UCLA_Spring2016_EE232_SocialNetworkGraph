library ("igraph")
library ("netrw")
## num_node is 100
num_node <- 100
#create a random undirected graph with 1000 nodes
g <- barabasi.game(num_node, directed = FALSE)

#calculate the distance between random pair of nodes of the graph
step <- 100
walkers <- num_node
walker.num<-walkers
ave<-rep(NA,step)
sdv<-rep(NA,step)
for(t in (1:step)){  
  
  rw <- netrw(g,walker.num,start.node=1:vcount(g),
              damping=1,weights=NULL,T=t,seed=NULL,
              output.walk.path=TRUE,output.walkers=0:(walker.num-1),
              #output.visit.prob=FALSE,output.nodes=0:(vcount(g)-1),
              output.device="memory",
              # walk.path.file="walk_path.txt",
              #visit.prob.file="visit_path.txt",
              local.pagerank=FALSE,teleport.prob=NULL)
  shortestPath<-rep(NA,walkers)
  for(w in (1:walkers))
  {
    #calculate the shortest paths from starting point to the t step node
    tmp <- get.shortest.paths(g,from=rw$walk.path[1,w],to=rw$walk.path[t,w])
    shortestPath[w] <-length(tmp$vpath[[1]])-1
  }
  ave[t]=mean(shortestPath)
  sdv[t]=sd(shortestPath)       	
}
png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2d_100_1.png")
plot(1:step,ave,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2d_100_2.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()
diameter1<-diameter(g)

## num_node is 10000
num_node <- 10000
#create a random undirected graph with 1000 nodes
g <- barabasi.game(num_node, directed = FALSE)

#calculate the distance between random pair of nodes of the graph
step <- 100
walkers <- num_node
walker.num<-walkers
ave<-rep(NA,step)
sdv<-rep(NA,step)
for(t in (1:step)){  
  
  rw <- netrw(g,walker.num,start.node=1:vcount(g),
              damping=1,weights=NULL,T=t,seed=NULL,
              output.walk.path=TRUE,output.walkers=0:(walker.num-1),
              #output.visit.prob=FALSE,output.nodes=0:(vcount(g)-1),
              output.device="memory",
              # walk.path.file="walk_path.txt",
              #visit.prob.file="visit_path.txt",
              local.pagerank=FALSE,teleport.prob=NULL)
  shortestPath<-rep(NA,walkers)
  for(w in (1:walkers))
  { 
    #calculate the shortest paths from starting point to the t step node
    tmp <- get.shortest.paths(g,from=rw$walk.path[1,w],to=rw$walk.path[t,w])
    shortestPath[w] <-length(tmp$vpath[[1]])-1
  }
  ave[t]=mean(shortestPath)
  sdv[t]=sd(shortestPath)         
}
png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2d_10000_1.png")
plot(1:step,ave,type="o",xlab="Steps t",ylab="Average Distance")
dev.off()
png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2d_10000_2.png")
plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
dev.off()
diameter2<-diameter(g)

