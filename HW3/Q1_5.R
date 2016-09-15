library("igraph")
g<-read.graph("/Users/ddyihai/Desktop/232e/hw3/sorted_directed_net.txt",format="ncol",directed=TRUE)


#1
is.connected(g)
clst<-clusters(g)
gccInd<-which.max(clst$csize)
nonGccID <-(1:vcount(g))[clst$membership != gccInd]
gcc<-delete.vertices(g,nonGccID)

#2
in_degree<-degree.distribution(gcc,mode="in")
out_degree<-degree.distribution(gcc,mode="out")
plot(in_degree,type="l",main="Indegree Distribution", xlab="degree",ylab="density")
plot(out_degree,type="l",main="Outdegree Distribution", xlab="degree",ylab="density")
hist(in_degree, breaks=200,main ="In-Degree Distribution", xlab="degree",ylab="density")
hist(out_degree, breaks=200,main ="Out-Degree Distribution", xlab="degree",ylab="density")

#3
#calculate the product of wij and wji, then do sqrt
gcc_U<-as.undirected(gcc,mode="collapse", edge.attr.comb=list(weight="prod"))
E(gcc_U)$weight<-sqrt(E(gcc_U)$weight)
#fast greedy
fgc<-fastgreedy.community(gcc_U)
sizes(fgc)
modularity(fgc)
plot(fgc,gcc_U,vertex.label=NA,main = "Community Structure Using Fast Greedy")
#label propagation
lpc<-label.propagation.community (gcc_U)
sizes(lpc)
modularity(lpc)
plot(lpc,gcc_U,vertex.label=NA,,vertex.size=10,main = "Community Structure Using Lable Propagation")

#4
maxfgcID<-which.max(sizes(fgc))
nonmaxfgcID <-(1:vcount(gcc_U))[fgc$membership != maxfgcID]
maxCom<-delete.vertices(gcc_U,nonmaxfgcID)
subfgc<-fastgreedy.community(maxCom)
sizes(subfgc)
modularity(subfgc)
plot(subfgc,maxCom,vertex.label=NA,vertex.size=10,main = "Sub Community Structure Using Fast Greedy")

#5
#fast greedy
ComIDfg<-which(sizes(fgc)>100)
subGraphfg<-list()
subMofg<-rep(NA,length(ComIDfg))
subSizefg<-list()
subComfg<-list()
subSizelllp<-list()
subComlllp<-list()
subMolllp<-rep(NA,length(ComIDfg))
for(i in 1:length(ComIDfg))
{ 
  nonIDfg <-(1:vcount(gcc_U))[fgc$membership != ComIDfg[i]]
  subGraphfg[[i]]<-delete.vertices(gcc_U,nonIDfg)
  subComfg[[i]]<-fastgreedy.community( subGraphfg[[i]])
  subSizefg[[i]]<-sizes(subComfg[[i]])
  subMofg[i]<-modularity(subComfg[[i]])
  
  subComlllp[[i]]<-label.propagation.community( subGraphfg[[i]])
  subSizelllp[[i]]<-sizes(subComlllp[[i]])
  subMolllp[i]<-modularity(subComlllp[[i]])
}


#label propagation
ComIDlp<-which(sizes(lpc)>100)
subGraphlp<-list()
subMolp<-rep(NA,length(ComIDlp))
subSizelp<-list()
subComlp<-list()
for(i in 1:length(subMolp))
{ 
  nonIDlp <-(1:vcount(gcc_U))[lpc$membership != ComIDlp[i]]
  subGraphlp[[i]]<-delete.vertices(gcc_U,nonIDlp)
  subComlp[[i]]<-label.propagation.community( subGraphlp[[i]])
  subSizelp[[i]]<-sizes(subComlp[[i]])
  subMolp[i]<-modularity(subComlp[[i]])
}
