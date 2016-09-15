library("igraph")
#Q4
g<-read.graph("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_edge_list.txt",format="ncol",directed=FALSE)
com <- fastgreedy.community(g)

#genreate id_genre
File_gene<-file("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_genre.txt",open="r")
g_line<-readLines(File_gene,1,encoding="uft-8")
nodeId<-0
Genre<-rep("",vcount(g))
while(length(g_line)!=0)
{
  gline=strsplit(g_line,"\t\t")
  nodeId<-(1:vcount(g))[V(g)$name==gline[[1]][1]]
  Genre[nodeId]<-gline[[1]][2]
  g_line<-readLines(File_gene,1,encoding="utf-8")
} 
close(File_gene)

#genreate id_name
File_name<-file("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/movie_list.txt",open="r")
n_line<-readLines(File_name,1,encoding="uft-8")
nodeId<-0
Name<-rep("",vcount(g))
while(length(n_line)!=0)
{
  nline=strsplit(n_line,"\t\t")
  nodeId<-(1:vcount(g))[V(g)$name==nline[[1]][2]]
  Name[nodeId]<-nline[[1]][1]
  n_line<-readLines(File_name,1,encoding="utf-8")
} 
close(File_name)

#genreate id_rating
File_rate<-file("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_rating.txt",open="r")
r_line<-readLines(File_rate,1,encoding="utf-8")
nodeId<-0
Rate<-rep("0",vcount(g))
while(length(r_line)!=0)
{
  rline=strsplit(r_line,"\t\t")
  nodeId<-(1:vcount(g))[V(g)$name==rline[[1]][1]]
  Rate[nodeId]<-rline[[1]][2]
  r_line<-readLines(File_rate,1,encoding="utf-8")
} 
close(File_rate)

#genreate id_director
File_dire<-file("/Users/gudazhong/Documents/UCLA_Course/EE232E/Project2/id_director.txt",open="r")
d_line<-readLines(File_dire,1,encoding="utf-8")
nodeId<-0
Director<-rep("0",vcount(g))
while(length(d_line)!=0)
{
  dline=strsplit(d_line,"\t\t")
  nodeId<-(1:vcount(g))[V(g)$name==dline[[1]][1]]
  Director[nodeId]<-dline[[1]][2]
  d_line<-readLines(File_dire,1,encoding="utf-8")
} 
close(File_dire)

#add attributes to nodes
V(g)$movieName<-Name
V(g)$Genre<-Genre
V(g)$Rate<-Rate
V(g)$Director<-Director

#Q5
sizes(com)
com_size<-0
gene_i<-list()
for(i in 1:length(sizes(com)))
{ 
  com_size<-sizes(com)[i]
  movie_i<-(1:vcount(g))[com$membership == i]
  movie_i_gene<-V(g)[movie_i]$Genre
  movie_i_gene<-table(movie_i_gene)
  gene_i[[i]]<-names(movie_i_gene[which(movie_i_gene>=(0.2*com_size))])
}

#Q6
movie_interest<-c("Batman v Superman: Dawn of Justice (2016)",
                  "Mission: Impossible - Rogue Nation (2015)",
                  "Minions (2015)")
nei<-list()
comId<-rep(0,3)
interestId<-rep(0,3)
for(i in 1:3)
{ 
  nodeID<-(1:vcount(g))[V(g)$movieName==movie_interest[i]]
  interestId[i]<-nodeID
  tmp<-neighborhood(g,1,V(g)[nodeID])
  nei[[i]]<-tmp[[1]][2:length(tmp[[1]])]
  print(movie_interest[i])
  edge_weight <- rep(0,length(nei[[i]]))
  for(j in 1:length(nei[[i]]))
  {
    edge_weight[j] <- g[from=nodeID,to=nei[[i]][j]]
  }
  names(edge_weight)<-nei[[i]]
  edge_weight <- sort(edge_weight,decreasing=TRUE)
  nei_id<-as.numeric(names(edge_weight[1:5]))
  nei_name<-V(g)[nei_id]$movieName
  print(nei_id)
  comId[i]<-com$membership[nodeID]
  print(comId[i])
  print(gene_i[[comId[i]]])
}