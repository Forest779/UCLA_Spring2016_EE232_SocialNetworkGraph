library("igraph")
library("netrw")

#(3a)
#generate graph
N=1000
p=0.01
g=random.graph.game(N,p,directed=TRUE)

degin=numeric(0)
degout=numeric(0)
#degree for directed graph
degin=degree(g,mode = "in")
degout=degree(g,mode = "out")

#random walk
r=netrw(g, damping=1,T=1000, output.walk.path=TRUE,output.visit.prob=TRUE)
par(mfrow=c(1,1))
vst=numeric(0)
vst=r$ave.visit.prob
#in
sumprob= numeric(max(degin))
count= numeric(max(degin))

for (i in 1:1000){
  sumprob[degin[i]]<-sumprob[degin[i]]+r$ave.visit.prob[i]
  count[degin[i]]<-count[degin[i]]+1
}
plotprob = numeric(0)
plotnum = numeric(0)
for (k in 1:max(degin))
{
  if(count[k] != 0){
    sumprob[k]=sumprob[k]/count[k]
    plotprob <- c(plotprob,sumprob[k])
    plotnum <- c(plotnum,k)
  }
}
plot(plotnum,plotprob,main="Relationship Between Prob and In-Degree",xlab="degree",ylab="prob",type="o")

relation<- rbind(degin,vst)
relation<- relation[,order(relation[1,])]  #order by degree
plot(relation[1,],relation[2,],xlab="Degree",ylab="Visit Probablity", main="Relationship Between Prob and In-Degree")

cor=cor(degin,vst)
print(cor)
plot(vst, main = "Probability of In-degree visit")

plot(r$ave.visit.prob[1:50],col="green",type="l",xlab="Node",yaxt="n",ylab="Average Visit Probability and Degree")
par(new="TRUE")
plot(degree(g,mode = c("in"))[1:50],col="red",type="l",xlab="Node",ylab="",yaxt="n") 

#out
sumprob= numeric(max(degout))
count= numeric(max(degout))

for (i in 1:1000){
  sumprob[degout[i]]<-sumprob[degout[i]]+r$ave.visit.prob[i]
  count[degout[i]]<-count[degout[i]]+1
}
plotprob = numeric(0)
plotnum = numeric(0)
for (k in 1:max(degout))
{
  if(count[k] != 0){
    sumprob[k]=sumprob[k]/count[k]
    plotprob <- c(plotprob,sumprob[k])
    plotnum <- c(plotnum,k)
  }
}
plot(plotnum,plotprob,main="Relationship Between Prob and Out-Degree",xlab="degree",ylab="prob",type="o")

relation<- rbind(degout,vst)
relation<- relation[,order(relation[1,])]  #order by degree
plot(relation[1,],relation[2,],xlab="Degree",ylab="Visit Probablity", main="Relationship Between Prob and Out-Degree")

corout=cor(degout,vst)
print(corout)
plot(vst, main = "Probability of Out-degree visit")

plot(r$ave.visit.prob[1:50],col="green",type="l",xlab="Node",yaxt="n",ylab="Average Visit Probability and Degree")
par(new="TRUE")
plot(degree(g,mode = c("out"))[1:50],col="red",type="l",xlab="Node",ylab="",yaxt="n") 

