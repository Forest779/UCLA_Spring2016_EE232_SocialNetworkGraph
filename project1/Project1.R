library("igraph")
library(ggplot2)
#load data
filePath = "/Users/ddyihai/Desktop/232e/project1/facebook_combined.txt"
g <-read.graph(filePath,format="ncol",directed=FALSE)

#Problem 1
connectivity <- is.connected(g)
dm <- diameter(g)
d <- degree.distribution(g)
dg<-d[2:length(d)]
plot(dg,type="l",main="Degree Distribution",xlab="Degree",ylab="density")
h = hist(degree(g), breaks=seq(0, by=1 , length.out=max(degree(g))+2))
#fit the model
ds <- data.frame(x=h$mids, y=h$density)
m<-nls(y~I(1/(a*x^power+b)),data=ds,start=list(power=1,a=1,b=0),trace=T)
summary(m)
power<-round(summary(m)$coefficient[1],3)
a<-round(summary(m)$coefficient[2],3)
b<-round(summary(m)$coefficient[3],3)
plot(dg,type="h",main="Fitted Power Model",xlab="Degree",ylab="Relative Frequency",sub="red:model;black:data")
lines(predict(m),lty=1,col="red")
text(10,0.02,paste("y=1/(",a,"*x^",power,"+",b,")",sep=""),pos=4)
#MSE
dat2 = data.frame(x=h$mids, y=(1/(0.0470285*(h$mids)^2.04151320+40.52869596)))
MSE2=sum((dat2$y-dat$y)^2)/max(degree(g))
aveDegree <- mean(degree(g))


#Problem 2
#find the subnet
personalNetwork <- graph.neighborhood(g,1,nodes=V(g))
subNet<-personalNetwork[[1]]
numOfNodes <- vcount(subNet)
numOfEdges <- ecount(subNet)
#plot personal network
nodeSize <- rep(3,vcount(subNet))
nodeSize[V(subNet)$name==V(g)[1]$name]=6
V(subNet)$color="magenta"
V(subNet)[V(subNet)$name==V(g)[1]$name]$color="black"
plot(subNet,main= "Personal Network of Node 1", vertex.size=nodeSize,vertex.label=NA,asp=9/16)


#Problem 3
#calculation
CoreNodes <- which(neighborhood.size(g,1)>201)
NumCores <- length(CoreNodes)
CoreNodeDegree <- numeric(0)
for(i in 1: NumCores){
  CoreNodeDegree <- c(CoreNodeDegree,length(neighbors(g,CoreNodes[i])))
}
ave_coreDegree <- mean(CoreNodeDegree)

#choose one core node
coreNode <- CoreNodes[1]
subNet<-personalNetwork[[coreNode]]
nodeSize <- rep(4,vcount(subNet))
nodeSize[V(subNet)$name==V(g)[coreNode]$name]=6
V(subNet)$color="skyblue"
V(subNet)[V(subNet)$name==V(g)[coreNode]$name]$color="black"
plot(subNet,main="Personal Network of coreNode 1",vertex.size=nodeSize,vertex.label=NA,asp=9/16)

#fast greedy commmunity
fg <-fastgreedy.community(subNet)
mod_fg <- modularity(subNet,fg$membership)
color_vec = fg$membership+1
plot(subNet,main="Personal Network of coreNode 1(fast greedy)",vertex.color=color_vec,vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl)
hist(fg$membership,main="Community Distribution of the Fast Greedy",xlab="Community Number",ylab="Numbers of Nodes")

#edge betweennness
eb <- edge.betweenness.community(subNet)
mod_eb <- modularity(subNet,eb$membership)
color_vec = eb$membership+1
plot(subNet,main="Personal Network of coreNode 1 (edge betweenness)",vertex.color=color_vec,vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl)
hist(eb$membership,main="Community Distribution of the Edge-Betweenness",xlab="Community Number",ylab="Numbers of Nodes")

#infomap community
im <- infomap.community(subNet)
mod_im <- modularity(subNet,im$membership)
color_vec = ic$membership+1
plot(subNet,main="Personal Network of coreNode 1 (infomap algorithm)",vertex.size=nodeSize,vertex.label=NA,layout=layout.lgl)
hist(im$membership,main="Community Distribution of the Infomap",vertex.color=color_vec,xlab="Community Number",ylab="Numbers of Nodes in a Community")


#Problem 4
#delete coreNode
coreInsub <- V(subNet)[V(subNet)$name==V(g)[coreNode]$name]
subNet_noCore <- delete.vertices(subNet,coreInsub)
V(subNet_noCore)$color="skyblue"
nodeSize <- rep(4,vcount(subNet_noCore))
plot(subNet_noCore,main="Personal Network of coreNode 1 without coreNode",vertex.size=nodeSize,vertex.label=NA,asp=9/16)

#fast greedy commmunity
fg_n <-fastgreedy.community(subNet_noCore)
mod_fg_n <- modularity(subNet_noCore,fg_n$membership)
V(subNet_noCore)$color <-rainbow(length(sizes(fg_n)))[fg_n$membership]
plot(subNet_noCore,main="Personal Network of coreNode 1(fast greedy)",vertex.size=nodeSize,vertex.label=NA)
hist(fg_n$membership,main="Community Distribution of the Fast Greedy",xlab="Community Number",ylab="Numbers of Nodes in a Community")

#edge betweennness community
eb_n <- edge.betweenness.community(subNet_noCore)
mod_eb_n <- modularity(subNet_noCore,eb_n$membership)
V(subNet_noCore)$color <-rainbow(length(sizes(eb_n)))[eb_n$membership]
plot(subNet_noCore,main="Personal Network of coreNode 1(edge betweenness)",vertex.size=nodeSize,vertex.label=NA)
hist(eb_n$membership,main="Community Distribution of the Edge-Betweenness",xlab="Community Number",ylab="Numbers of Nodes in a Community")

#infomap community
im_n <- infomap.community(subNet_noCore)
mod_im_n <- modularity(subNet_noCore,im_n$membership)
V(subNet_noCore)$color <-rainbow(length(sizes(im_n)))[im_n$membership]
plot(subNet_noCore,main="Personal Network of coreNode 1(infomap algorithm)",vertex.size=nodeSize,vertex.label=NA)
hist(im_n$membership,main="Community Distribution of the Infomap",xlab="Community Number",ylab="Numbers of Nodes in a Community")


############Part5############
g_fb <-read.graph("/Users/YANNAN/Desktop/232_proj1/facebook_combined.txt",format="ncol",directed=FALSE)

personalNetwork <- graph.neighborhood(g_fb,1,nodes=V(g_fb))
coreNodes <- which(neighborhood.size(g_fb,1)>201)#index
numOfcoreNodes <- length(coreNodes)

embeddness<-NULL
dispersion<-NULL
pickNode1 <- 5
pickNode2 <- 19
pickNode3 <- 27
q<-1


for(i in 1:numOfcoreNodes)
{  
  core_i<-coreNodes[i]
  pn_i <- personalNetwork[[core_i]]
  coreI <- V(pn_i)[V(pn_i)$name==V(g_fb)[core_i]$name]#name
  pn_no_i <- delete.vertices(pn_i,coreI)  
  
  embeddness_i<-as.vector(degree(pn_no_i))
  dispersion_i<-rep(0,vcount(pn_no_i))
  #sub_pn <- graph.neighborhood(pn_no_i,2,nodes=V(pn_no_i))
  sub_pn_1 <- graph.neighborhood(pn_no_i,1,nodes=V(pn_no_i))
  for(j in 1:vcount(pn_no_i))
  {
    #sub_pn_j<-sub_pn[[j]]
    sub_pn_j_1<-sub_pn_1[[j]]
    #coreJ <- V(sub_pn_j)[V(sub_pn_j)$name==V(pn_no_i)[j]$name]
    #sub_pn_no_j <- delete.vertices(sub_pn_j,coreJ)
    coreJ_1 <- V(sub_pn_j_1)[V(sub_pn_j_1)$name==V(pn_no_i)[j]$name]
    sub_pn_no_j_1 <- delete.vertices(sub_pn_j_1,coreJ_1)
    #V <- V(sub_pn_no_j)[is.element(V(sub_pn_no_j)$name, V(sub_pn_no_j_1)$name)]
    #betweenDistance <-shortest.paths(sub_pn_no_j,v=V,to=V)
    betweenDistance <-shortest.paths(sub_pn_no_j_1,v=V(sub_pn_no_j_1),to=V(sub_pn_no_j_1))
    dispersion_i[j]=0.5*length(which(betweenDistance>2))
    
  } 
  
    if(i==pickNode1 || i==pickNode2 || i==pickNode3)
    {
      max_e<-which.max(embeddness_i)
      max_p<-which.max(dispersion_i)
      max_ep<-which.max(embeddness_i/dispersion_i)
      node_e<-V(pn_i)[V(pn_i)$name==V(pn_no_i)[max_e]$name]
      node_p<-V(pn_i)[V(pn_i)$name==V(pn_no_i)[max_p]$name]  
      node_ep<-V(pn_i)[V(pn_i)$name==V(pn_no_i)[max_ep]$name]
      
      nodeSize <- rep(2,vcount(pn_i))
      nodeSize[coreI]<-6
      nodeSize[node_e]<-6
      nodeSize[node_p]<-6
      nodeSize[node_ep]<-6
      
      nodeLabel <- rep(NA,vcount(pn_i))
      nodeLabel[coreI]<-"core"
      nodeLabel [node_e]<-"max_e"
      nodeLabel [node_p]<-"max_p"
      nodeLabel [node_ep]<-"max_r"
      #V(pn_i)$color="skyblue"
      #V(pn_i)[coreI]$color="black"
      #V(pn_i)[node_e]$color="red"
      #V(pn_i)[node_p]$color="yellow"
      #V(pn_i)[node_ep]$color="green"
      
      
      E(pn_i)$color <-"#80808020"
      E(pn_i)[from(node_e)]$color="red"
      E(pn_i)[from(node_e)]$width=2
      E(pn_i)[from(node_p)]$color="yellow"
      E(pn_i)[from(node_p)]$width=2
      E(pn_i)[from(node_ep)]$color="green"
      E(pn_i)[from(node_ep)]$width=2
      
      com<-fastgreedy.community(pn_i)
      
      V(pn_i)$color <-rainbow(length(sizes(com)))[com$membership]
      E(pn_i)$weight="3"
      
      
      
      
      
      
      E(pn_i)$weight="3"
      if(q==1){
        png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_1.png")
        plot(pn_i,vertex.label=nodeLabel,main="Highlighted Vertices and Edges of Picked Personal Network",vertex.size=nodeSize,vertex.label=nodeLabel,asp=9/16,layout=layout.lgl)
        dev.off()      
      }
      if (q==2)
      {
        png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_2.png")
        plot(pn_i,vertex.label=nodeLabel,main="Highlighted Vertices and Edges of Picked Personal Network",vertex.size=nodeSize,vertex.label=nodeLabel,asp=9/16,layout=layout.lgl)
        dev.off()
      }
      if (q==3) 
      {
        png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_3.png")
        plot(pn_i,vertex.label=nodeLabel,main="Highlighted Vertices and Edges of Picked Personal Network",vertex.size=nodeSize,vertex.label=nodeLabel,asp=9/16,layout=layout.lgl)
        dev.off()
      }
      q=q+1
    }
    
    embeddness<-append(embeddness,embeddness_i)
  dispersion<-append(dispersion,dispersion_i)
  
  
}

h_e <- hist (embeddness, breaks=seq (-0.5, by=1, length.out=max(embeddness) +2))
ed <- data.frame(x=h_e$mids,y=h_e$density)
png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_4.png")
plot(ed,type='l',main ="Embeddedness Distribution", xlab="Embeddedness",ylab="Frequency")
dev.off()
png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_5.png")
plot(embeddness,main="Embeddedness",type="l")
dev.off()

h_d <- hist (dispersion, breaks=seq (-0.5, by=1, length.out=max(dispersion) +2))
disd <- data.frame(x=h_d$mids,y=h_d$density)
png(filename="/Users/YANNAN/Desktop/232_proj1/dispersion.png")
plot(disd,type="l", main ="Dispersion Distribution", xlab="Dispersion",ylab="Frequency")
dev.off()
png(filename="/Users/YANNAN/Desktop/232_proj1/Q5_7.png")
plot(dispersion,main = "Dispersion", type="l")
dev.off()

#######Problem6#########
edgelistFile<-"/Users/YANNAN/Desktop/232_proj1/facebook_combined.txt"
g <- read.graph(edgelistFile , format = "ncol" , directed=FALSE)
coreNodes <- which(neighborhood.size(g, 1 , nodes=V(g)) > 201)
community_acquintance_id = numeric(0)
community_close_friend_id = numeric(0)
j=1
for (core_i in 1:length(coreNodes))
{
  core <- coreNodes[core_i]
  subGraphNodes <- neighborhood(g , 1 , nodes=core)
  subGraphNodes <- subGraphNodes[[1]]
  nonSubGraphNodes <- which(!((1:vcount(g)) %in% subGraphNodes))
  subGraph <- delete.vertices(g , nonSubGraphNodes)
  fg <- fastgreedy.community(subGraph)
  community_vector=numeric(0)
  ratio_vector=numeric(0)
  subMO <-rep(0,length(sizes(fg)))
  
  for (i in 1:length(fg)) {   
    communityNodes <- V(subGraph)$name[which(fg$membership==i)]
    non_communityNodes <- V(subGraph)$name[which(fg$membership!=i)]
    if (length(communityNodes) >= 10){
      communityGraph <- delete.vertices(subGraph, non_communityNodes)
      dgr_min <- min(degree(communityGraph))
      ratio <- dgr_min / length (communityGraph)
      ratio1<-ecount(communityGraph)/vcount(communityGraph)
      ratio_vector=c(ratio_vector,ratio1)
      subMO[i] <- modularity(communityGraph,fg$membership)#modularity
    }
  }
  community_submo_min_number=which.min(subMO)
  community_submo_max_number=which.max(subMO)
  non_min_communityNodes <- V(subGraph)$name[which(fg$membership!=community_submo_min_number)]
  non_max_communityNodes <- V(subGraph)$name[which(fg$membership!=community_submo_max_number)]
  community_minGraph <- delete.vertices(subGraph, non_min_communityNodes)
  community_maxGraph <- delete.vertices(subGraph, non_max_communityNodes)
  jpeg(paste(6,'Core #',core_i,' Acquintance.jpg'), width = 800, height = 600)
  plot(community_minGraph,main=paste('Acquintance Community of Core #',core_i))
  dev.off()
  jpeg(paste(6,'Core #',core_i,' Close Friends.jpg'), width = 800, height = 600)
  plot(community_maxGraph,main=paste('Close Friends Community of Core #',core_i))
  dev.off()
  
    community_acquintance_number=which.min(ratio_vector)
    community_close_friends_number=which.max(ratio_vector)
    community_acquintance_id[j]=community_acquintance_number
    community_close_friend_id[j]=community_close_friends_number
    j=j+1
    non_acquintance_communityNodes <- V(subGraph)$name[which(fg$membership!=community_acquintance_number)]
    non_close_friends_communityNodes <- V(subGraph)$name[which(fg$membership!=community_close_friends_number)]
    community_acquintanceGraph <- delete.vertices(subGraph, non_acquintance_communityNodes )
    community_close_friendsGraph <- delete.vertices(subGraph, non_close_friends_communityNodes )
    jpeg(paste(6,'Core #',core_i,' Acquintance.jpg'), width = 800, height = 600)
    plot(community_acquintanceGraph,main=paste('Acquintance Community of Core #',core_i))
    dev.off()
    jpeg(paste(6,'Core #',core_i,' Close Friends.jpg'), width = 800, height = 600)
    plot(community_close_friendsGraph,main=paste('Close Friends Community of Core #',core_i))
    dev.off()
  
}
###########Problem7#########
#read all the files
Path <- "/Users/gudazhong/Documents/UCLA_Course/EE232E/Project1/gplus/"
circlesFiles <- list.files(Path,pattern="*.circles",full.names=FALSE)
edgesFiles <- list.files(Path,pattern="*.edges",full.names=FALSE)
nodeIndex <- strsplit(circlesFiles,".circles")

g_gp<-list()
circle<-list()
wtc<-list()
imc<-list()

#read the file one by one
k<-1
for(i in 1:length(nodeIndex))
{
  circlesFile <- paste(Path,nodeIndex[[i]],".circles",sep="")
  fileConnection <- file(circlesFile, open="r")
  lines<-readLines(fileConnection)
  close(fileConnection)
  
  circles <-strsplit(lines,"\t")
  
  #only pick up nodes with more than 2 circles
  if(length(circles)>2)
  {
    edgeFile <- paste(Path,nodeIndex[[i]],".edges",sep="")
    
    #g_noCore is the origin graph in file which doesn't has the core node
    g_noCore <-read.graph(edgeFile,format="ncol",directed=TRUE)
    g_noCore<-g_noCore+vertex(nodeIndex[[i]],color="black")
    g_noCore[vcount(g_noCore),1:(vcount(g_noCore)-1)]<-1
    
    #g_gp is a graph we add the core in it
    #we store the graph, circle for each node we pick up
    g_gp[[k]]<-g_noCore
    circle[[k]]<-circles
    
    #we store the community inforamtion of two methods for each node we pick up
    wtc[[k]] <- walktrap.community(g_noCore)
    imc[[k]] <- infomap.community (g_noCore)
    
    k=k+1
  }
}

#sample node5 plot
hist(wtc[[5]]$membership,col="lightblue",main="Community Distribution of the walktrap Algorithm",xlab="Community Number",ylab="Numbers of Nodes in a Community")
hist(imc[[5]]$membership,col="lightgreen",main="Community Distribution of the infomap Algorithm",xlab="Community Number",ylab="Numbers of Nodes in a Community")
circle5 <- array()
q <- 1
for (i in 1:length(circle[[5]])) {
  for (j in 1:length(circle[[5]][[i]])) {
    circle5[[q]] <- i
    q <- q+1
  }
}
hist(circle5,col="pink",main="Original Circle Distribution of the infomap Algorithm",xlab="Circle Number",ylab="Numbers of Nodes in a Circle")



#use Overlap rate to calculate the overlap between the communities and the circles
###walktrap algorithm
allOR_wt<-rep(0,k-1)

for(n in 1:(k-1)){
  nodeNum<-vcount(g_gp[[n]])
  map<-rep(0,length(sizes(wtc[[n]])))
  OR<-rep(0,length(sizes(wtc[[n]])))
  for(i in 1:length(sizes(wtc[[n]])))
  {
    communityNode<- (wtc[[n]])$names[which(wtc[[n]]$membership==i)]
    match<-rep(0,length(circle[[n]]))
    for(j in 1:length(circle[[n]])){
      circleNode <- circle[[n]][[j]] 
      #nodenum in both community and circle
      CinC <- length(intersect(communityNode,circleNode))
      Com <- length(communityNode)
      Cir <- length(circleNode)-1
      #calculate the OR of the map
      match[j]<- CinC/(Com+Cir)
    }
    map[i]<-which.max(match)
    OR[i]<-match[map[i]]
  }
  allOR_wt[n]<-sum(OR)
}

###infomap algorithm

allOR_im<-rep(0,k-1)

for(n in 1:(k-1)){
  nodeNum<-vcount(g_gp[[n]])
  map<-rep(0,length(sizes(imc[[n]])))
  OR<-rep(0,length(sizes(imc[[n]])))
  for(i in 1:length(sizes(imc[[n]])))
  {
    communityNode<- (imc[[n]])$names[which(imc[[n]]$membership==i)]
    match<-rep(0,length(circle[[n]]))
    for(j in 1:length(circle[[n]])){
      circleNode <- circle[[n]][[j]] 
      #Cinc means nodenum in both community and circle
      CinC <- length(intersect(communityNode,circleNode))
      Com <- length(communityNode)
      Cir <- length(circleNode)-1
      #calculate the OR of the map
      match[j]<- CinC/(Com+Cir)
    }
    map[i]<-which.max(match)
    OR[i]<-match[map[i]]
  }
  allOR_im[n]<-sum(OR)
}

plot(allOR_wt, type = "l", main = "Overlap on walktrap detected communities", xlab = "index", ylab = "Overlap Rate")
plot(allOR_im, type = "l", main = "Overlap on Infomap detected communities", xlab = "index", ylab = "Overlap RateR")






