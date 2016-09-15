library ("igraph")
library ("netrw")
num_node <- 1000
#create a random undirected graph with 1000 nodes
g <- barabasi.game(num_node, power = -3,  directed = FALSE)
d1<-degree.distribution(g)
d1<-d1[2:length(d1)]
#png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2a_001.png")
plot(d1, type="o")
#dev.off()

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
# png(filename="/Users/Robert/Google Drive/EE232E/HW2/Q2b_1.png")
# plot(1:step,ave,type="o",xlab="Steps t",ylab="Average Distance")
# dev.off()
# png(filename="/Users/Robert/Google Drive/EE232E/HW2/Q2b_2.png")
# plot(1:step,sdv,type="o",xlab="Steps t",ylab="Standard Deviation")
# dev.off()

dd <-rep(NA,walkers)
discount <- 1/walkers
for(w in (1:walkers))
{
  # the degree of the end node of the random walk
  dd[w] <- degree(g,rw$walk.path[step,w])
}
png(filename="C:\\Users\\Ñâéª\\Desktop\\232\\Q2e.png")
#hs<-hist(dd,breaks=seq(-0.5, by=1 , length.out=max(dd)+2))
dd<-data.frame(x=hs$mids[2:13], y=hs$density[2:13])
#plot(dd, type="o")
#plot(dd,type='o')
dev.off()
diameter(g)
