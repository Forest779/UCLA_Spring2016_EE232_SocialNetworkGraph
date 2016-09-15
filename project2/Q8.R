library("hash")

#PROBLEM 8
load("/Users/ddyihai/Desktop/232e/project2/Project2/Project2_1-6.RData")
g_a<-read.graph("/Users/ddyihai/Desktop/232e/project2/Project2/edge_list.txt",format="ncol",directed=TRUE)
File_actorID<-file("/Users/ddyihai/Desktop/232e/project2/Project2/actor_name_id_map.txt",open="r")

p_rScore<-page.rank (g_a, algo = c("prpack", "arpack", "power"),
                     directed = TRUE, damping = 0.85,
                     weights = NULL) 
p_rScore1<-sort(p_rScore$vector,decreasing=TRUE)


#########hashmap is not good than normal list
movieid_to_nodeNum = hash()
movieid_to_nodeNum <- list()
for(i in 1:length(V(g)$name)){
  print(i)
  movieid_to_nodeNum[V(g)$name[i]] = i
}

movieid_nodeNum <- list()
for(i in 1:length(V(g)$name)){
  print(i)
  tp = as.numeric( attributes(movieid_to_nodeNum[i]) )
  movieid_nodeNum[tp] = i
}


File_name<-file("/Users/ddyihai/Desktop/232e/project2/Project2/movie_list.txt",open="r")
n_line<-readLines(File_name,1,encoding="latin")

nodeId<-0
a_rank<-list()
actorList<-list()
i=0
nodeid_l <- list()
movieid_l <- list
while(length(n_line)!=0)
{
    nline=strsplit(n_line,"\t\t")
    tp <- as.numeric(nline[[1]][2])
    nodeId<-movieid_nodeNum[tp][[1]]
    if(is.na(V(g)$Rate[nodeId]) == FALSE){
      actors<-nline[[1]][3:length(nline[[1]])]
      if(length(nodeId) != 0){
        actorRank_tmp<-c()
          for(j in 1:length(actors))
          { 
            r<-p_rScore1[which(names(p_rScore1)==actors[j])]
            actorRank_tmp<-append(actorRank_tmp,r)  
          }
          actorRank_tmp <- sort(actorRank_tmp,decreasing = TRUE)
          a_rank[[nodeId]]<-actorRank_tmp[1:5]
          nodeid_l <- c(nodeid_l, nodeId)
          movieid_l <- c(movieid_l,tp)
          print(i)
          i=i+1
      }
    }
  n_line<-readLines(File_name,1,encoding="latin1")
}
close(File_name)




File_top<-file("/Users/ddyihai/Desktop/232e/project2/Project2/top_100_director.txt",open="r")
File_director<-file("/Users/ddyihai/Desktop/232e/project2/Project2/movieid_director.txt",open="r")
top<-readLines(File_top,encoding="latin1")
d_line<-readLines(File_director,1,encoding="latin1")
nodeId<-0
Director<-rep("",vcount(g))
i=0
while(length(d_line)!=0)
{
  print(i)
  i = i+1
  dline=strsplit(d_line,"\t\t")
  tp <- as.numeric(dline[[1]][1])
  if(tp != 0){
    nodeId<-movieid_nodeNum[tp][[1]]
    if(length(nodeId) != 0){
      Director[nodeId]<-dline[[1]][2]
    }
  }
  d_line<-readLines(File_director,1,encoding="latin1")
}
V(g)$Director<-Director
close(File_director)
close(File_top)


topDirector<-unique(top)[1:100]
boDirect<-list()
label<-list()
res <- list()

for(i in 1:length(nodeid_l))
{ print(i)
  nodeid_last = nodeid_l[i][[1]]
    tmpDirect<-rep(0,106)
    if(V(g)[nodeid_last]$Director %in% topDirector)
    {
      tmpDirect[101]<-1
      j<-which(topDirector==V(g)[nodeid_last]$Director)
      tmpDirect[j]<-1
    }
    tmpDirect[102:106] <- a_rank[[ nodeid_last ]][1:5]
    
    boDirect[[nodeid_last]]<-tmpDirect
    
    res[[i]] <- tmpDirect
    if(is.na(V(g)$Rate[nodeid_last])){
      label[[i]] <- 0
    }
    else{
      label[[i]] <- V(g)$Rate[nodeid_last]
    }
}

lapply(res, write, "/Users/ddyihai/Desktop/232e/project2/Project2/mldata/trainwhole_nodeid.txt", append=TRUE, ncolumns=106)
lapply(label, write, "/Users/ddyihai/Desktop/232e/project2/Project2/mldata/labelwhole_nodeid.txt", append=TRUE, ncolumns=1)

#set.seed(204593426)
#ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#model <- train(res, label, method = "lm", trControl = ctrl)
#print(model)