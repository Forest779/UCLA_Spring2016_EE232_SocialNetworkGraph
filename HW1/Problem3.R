library(igraph)

#(a)
g <- aging.prefatt.game(1000,1,-1,aging.bin = 1000,directed = F)
#g <- growing.random.game(1000,1,-1,directed = F)
xx = 1:length(degree.distribution(g))
degreeVector = degree(g);
h <- hist(degreeVector, freq = FALSE, seq(by=1,length.out=max(degreeVector)+2),main = "Degree Distribution" ,xlab="degree",ylab="density")

plot(degree.distribution(g),type = 'h',main = "Degree Distribution",xlab="degree",ylab="density")
lines(degree.distribution(g), type="o")

#(b)
numberCommun = numeric(0)
avemod = numeric(0)
for(i in 1:100){
  g <- aging.prefatt.game(1000,1,-1,aging.bin = 1000,directed = F)
  x = fastgreedy.community(g)
  mod = modularity(x)
  numberCommun = c(numberCommun,length(x))
  avemod = c(avemod,mod)
}
plot(x, g, col = membership(x),mark.groups = communities(x), edge.color = c("black", "red")[crossing(x,g) + 1],vertex.label=NA)
#find the biggest community
numOfCom = length(x)
NumInEachCommunity = rep(0,numOfCom)
for(i in 1:numOfCom){
  NumInEachCommunity[i] = length(which(x$membership == i))
}
maxNum = max(NumInEachCommunity)
h <- hist(x$membership, freq = FALSE, seq(0,max(x$membership),by=1),main = "Community Distribution" ,xlab="degree",ylab="density")
Maxcommunity = which(NumInEachCommunity == maxNum)
Subgraph <- subgraph(g, which(x$membership == Maxcommunity[1]))
plot(Subgraph,vertex.label=NA)
cat("number of communities = ", mean(numberCommun))
cat("modularity = ",mean(avemod))


