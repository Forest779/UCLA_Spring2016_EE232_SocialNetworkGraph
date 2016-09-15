library("igraph")
g<-read.graph("/Users/gudazhong/Documents/UCLA_Course/EE232E/hw3/sorted_directed_net.txt",format="ncol",directed=TRUE)

#find the largest connected structure
cl<-clusters(g)
gccID<-which.max(cl$csize)
nonGccID <-(1:vcount(g))[cl$membership != gccID]
gcc<-delete.vertices(g,nonGccID) #gcc is the largest connected structure

#find the communities by fast greedy algorithm
gcc_U<-as.undirected(gcc,mode="collapse", edge.attr.comb=list(weight="prod"))
E(gcc_U)$weight<-sqrt(E(gcc_U)$weight)
fgc<-fastgreedy.community(gcc_U) #fgc contains the result of the communities

#The work for question 6
library("netrw")

#build m for every node
m <- matrix(0,vcount(gcc),length(sizes(fgc)))
for(i in 1:vcount(gcc))
{
  m[i,fgc$membership[i]]=1
}

#build M for every nodes
M <- matrix(0,vcount(gcc),length(sizes(fgc)))

#calculate vj and M for every node
walker.num <-1000
step<-100
n_vj <- 30
for(i in 1:vcount(gcc))
{
  randWalk<-netrw(gcc,walker.num,start.node<-i,
                  damping=0.85, T=step, 
                  output.walk.path=TRUE, output.walkers=0:(walker.num-1),
                  output.visit.prob=TRUE, output.nodes=0:(vcount(gcc)-1),
                  output.device="memory")
 visitProb<-randWalk$ave.visit.prob
 sortVP<-sort(visitProb,index.return=TRUE,decreasing=TRUE)
 for(j in 1:n_vj)
 {
   M[i,]=M[i,]+sortVP$x[j]*m[sortVP$ix[j],]
 }
}

#find the second largest membership value for every nodes.
second_largest <- matrix(0,vcount(gcc),1)
for(i in 1:vcount(gcc))
{
  second_largest[i,1] = sort(M[i,], decreasing = TRUE)[2]
}

#The distribution of the second largest membership value
hist(second_largest, xlab = "Second largest membership value", ylab = "Frequency")

#set a threshold THR, and find all the nodes whose second_largest membership value is bigger than THR
THR <- 0.02
filtered <- list() #filtered stored the nodes whose second_largest membership value is bigger than THR
num_filtered <- 0 #how many elements in the filtered
for(i in 1:vcount(gcc))
{
  if (second_largest[i,1] >= THR)
  {
    filtered<- rbind(filtered,i)
    num_filtered<-num_filtered+1
  }
}












