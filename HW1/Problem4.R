library("igraph")
#(a)
NodesNum = 1000
Diameter = numeric(0)
for (i in 1:100){
  g = forest.fire.game(NodesNum, 0.3, 0.3, 1, directed=TRUE)
  Diameter = c (Diameter, diameter(g))
}
VectorDegreeIn = degree(g,mode="in")
hin <- hist(VectorDegreeIn, freq = FALSE, seq(0,max(VectorDegreeIn),by=1),main = "In Degree Distribution" ,xlab="degree",ylab="density")
VectorDegreeOut = degree(g,mode="in")
hout <- hist(VectorDegreeOut, freq = FALSE, seq(0,max(VectorDegreeOut),by=1),main = "Out Degree Distribution" ,xlab="degree",ylab="density")

#(b)
D_avg = mean(Diameter)

#(c)
numberCommun = numeric(0)
avemod = numeric(0)
for(i in 1:100){
  #g = forest.fire.game(NodesNum, fw.prob = 0.37, bw.factor = 0.32/0.37, directed=TRUE)
  g = forest.fire.game(NodesNum, 0.3, 0.3, 1, directed=TRUE)
  structure = walktrap.community(g)
  mod = modularity(structure)
  numberCommun = c(numberCommun,length(x))
  avemod = c(avemod,mod)
}
#find the biggest community
numOfCom = length(structure)
NumInEachCommunity = rep(0,numOfCom)
for(i in 1:numOfCom){
  NumInEachCommunity[i] = length(which(x$membership == i))
}
maxNum = max(NumInEachCommunity)
h <- hist(structure$membership, freq = FALSE, seq(0,max(structure$membership),by=1),main = "Community Distribution" ,xlab="degree",ylab="density")
Maxcommunity = which(NumInEachCommunity == maxNum)
Subgraph <- subgraph(g, which(structure$membership == Maxcommunity[1]))
plot(Subgraph,vertex.label=NA)
plot(structure,g,vertex.label=NA)

cat("diameter = ",D_avg)
cat("number of communities = ", mean(numberCommun))
cat("modularity = ",mean(avemod))
