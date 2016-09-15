library("igraph")
library("hash")
library(e1071)
#################Q2 Q3####################
g<-read.graph("/Users/YANNAN/Desktop/232_Project2/edge_list.txt",format="ncol",directed=TRUE)
File_actorID<-file("/Users/YANNAN/Desktop/232_Project2/actor_name_id_map.txt",open="r")
actorID_map<-readLines(File_actorID,encoding="utf-8")

p_rScore<-page.rank (g, algo = c("prpack", "arpack", "power"),
                     directed = TRUE, damping = 0.85,
                     weights = NULL) 
p_rScore1<-sort(p_rScore$vector,decreasing=TRUE)
id<-NULL
for(i in 1:10)
{
  id<-as.numeric(names(p_rScore1[i]))
  actor_ID<-actorID_map[id+1]
  aline=strsplit(actor_ID,"\t\t")
  print(aline[[1]][1])
  print(p_rScore1[i])
}

#########chose my movie celebrities##########
namelist<-c("Streep, Meryl","Foster, Jodie","Winslet, Kate","Blanchett, Cate","Kidman, Nicole","De Niro, Robert","Eastwood, Clint","Spacey, Kevin","Depp, Johnny","Firth, Colin")
actorID<-c(232146,181371,241737,163040,196168,33602,39579,131743,35091,44421)
actor_score<-hash()
for(i in 1:length(p_rScore$vector))
{
  for(j in 1:10)
  {
    id<-actorID[j]
    if(names(p_rScore$vector[i])==id)
    {
      .set(actor_score,keys=namelist[j],values=p_rScore$vector[i])
    }
  }
}
print(values(actor_score,keys=namelist))

close(File_actorID)


##########significant pairs########
g_U<-as.undirected(g,mode="collapse",edge.attr.comb=list(weight="mean"))
sorted_weight<-sort(E(g_U)$weight,index.return=TRUE,decreasing=TRUE)
#sap <- list()
for(i in 1:10)
{
  sap=sorted_weight$ix[i]
  #print(sorted_weight$x[i])
  print(E(g_U)[sap])
}

nam<-c(26160,90939,44248,62607,195974,195970,42777,42779,61991,140638,61991,140637,125429,36769,88493,36769,2885,114465,104253,67459)
id<-NULL
for(i in 1:length(nam)){
  idx<-nam[i]
  id<-as.numeric(V(g_U)[idx]$name)
  actor_ID<-actorID_map[id+1]
  aline=strsplit(actor_ID,"\t\t")
  print(aline[[1]][1])
}

