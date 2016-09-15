library("igraph")
library("netrw")

#(3a)
#generate graph
N=1000
p=0.01
g=random.graph.game(N,p,directed=FALSE)

deg=numeric(0)
#degree for undirected graph
deg=degree(g)

#random walk
r=netrw(g, damping=1,T=1000, output.walk.path=TRUE,output.visit.prob=TRUE)
par(mfrow=c(1,1))
vst=numeric(0)
vst=r$ave.visit.prob

sumprob= numeric(max(deg))
count= numeric(max(deg))

for (i in 1:1000){
  sumprob[deg[i]]<-sumprob[deg[i]]+r$ave.visit.prob[i]
  count[deg[i]]<-count[deg[i]]+1
}
plotprob = numeric(0)
plotnum = numeric(0)
for (k in 1:max(deg))
{
  if(count[k] != 0){
    sumprob[k]=sumprob[k]/count[k]
    plotprob <- c(plotprob,sumprob[k])
    plotnum <- c(plotnum,k)
  }
}
plot(plotnum,plotprob,main="Relationship Between Prob and Degree",xlab="degree",ylab="prob",type="o")

relation<- rbind(deg,vst)
relation<- relation[,order(relation[1,])]  #order by degree
plot(relation[1,],relation[2,],xlab="Degree",ylab="Visit Probablity", main="Relationship Between Prob and Degree")

cor=cor(deg,vst)
print(cor)
plot(vst, main = "Probability")


plot(r$ave.visit.prob[1:50],col="green",type="l",xlab="Node",yaxt="n",ylab="Average Visit Probability and Degree")
par(new="TRUE")
plot(degree(g)[1:50],col="red",type="l",xlab="Node",ylab="",yaxt="n") 


