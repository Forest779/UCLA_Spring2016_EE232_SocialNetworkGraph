library("igraph")
load("/Users/ddyihai/Desktop/232e/project2/Project2/Project2_1-6.RData")
#PROBLEM 7
movie_interest<-c("Batman v Superman: Dawn of Justice (2016)",
                  "Mission: Impossible - Rogue Nation (2015)",
                  "Minions (2015)")

neighbor<-list()
edgeWeight<-list()
comunityId<-rep(0,3)
#find neighbor and community
for(i in 1:3)
{ 
  nodeID<-(1:vcount(g))[V(g)$nodeID==movie_interest[i]]
  tmp<-neighborhood(g,1,V(g)[nodeID])
  neighbor[[i]]<-tmp[[1]][2:length(tmp[[1]])]
  print(movie_interest[i])
  edge_weight <- rep(0,length(neighbor[[i]]))
  for(j in 1:length(neighbor[[i]]))
  {
    edge_weight[j] <- g[from=nodeID,to=neighbor[[i]][j]]
  }
  names(edge_weight)<-neighbor[[i]]
  edgeWeight[[i]]<- edge_weight
  edge_weight <- sort(edge_weight,decreasing=TRUE)
  nei_name<-as.numeric(names(edge_weight[1:5]))
  nei_name<-V(g)[nei_name]$movieName
  print(nei_name)
  comunityId[i]<-com$membership[nodeID]
  print(comunityId[i])
}


for(i in 1:3) 
{
  nodeS<-neighbor[[i]]
  neighborR1<-V(g)[nodeS]$Rate
  neighborR1<-as.numeric(neighborR1)
  neighborR1<-neighborR1[which(neighborR1!=0)]
  comunityNode<-(1:vcount(g))[com$membership==comunityId[i]]
  comunityR2<-V(g)[comunityNode]$Rate
  comunityR2<-as.numeric(comunityR2)
  comunityR2<-comunityR2[which(comunityR2!=0)]
  r1<-mean(neighborR1)
  r2<-mean(comunityR2) 
  edgeWeight[[i]]<-edgeWeight[[i]]/sum(edgeWeight[[i]])
  a<-as.numeric(V(g)[nodeS]$Rate)[-which(is.na(as.numeric(V(g)[nodeS]$Rate)))]
  b<-edgeWeight[[i]][-which(is.na(as.numeric(V(g)[nodeS]$Rate)))]
  r1_p<- sum(a*b)
  
  #calculate tge score
  score_simple1<-0.5*r1+0.5*r2
  score_simple2<-(sum(neighborR1)+sum(comunityR2))/(length(neighborR1)+length(comunityR2))
  score_simple3<-sqrt((r1*r1+r2*r2)/2)
  score_simple4<-sqrt(r1*r2)
  score_weight1<-0.5*r1_p+0.5*r2
  score_weight2<-(r1_p*length(neighborR1)+sum(comunityR2))/(length(neighborR1)+length(comunityR2))
  score_weight3<-sqrt((r1_p*r1_p+r2*r2)/2)
  score_weight4<-sqrt(r1_p*r2)
  
  print(movie_interest[i])
  print(score_simple1)
  print(score_simple2)
  print(score_simple3)
  print(score_simple4)
  print(score_weight1)
  print(score_weight2)
  print(score_weight3)
  print(score_weight4)
}


