library ("igraph")
#(a)
p = 1:3
p[1] = 0.01
p[2] = 0.05
p[3] = 0.1
nodesNum = 1000
#g = c()
#dg = c()
isConnected = numeric(0)
for(i in 1:3){
  print(i)
  g = random.graph.game(nodesNum,p[i],directed=FALSE)
  dg = degree.distribution(g)
  degreeVector = degree(g)
  title = paste( "Degree Distribution with p = ",p[i])
  h <- hist(degreeVector, freq = FALSE, seq(-0.5,by=1,length.out=max(degreeVector)+2),main = title ,xlab="degree",ylab="density")
}

#(b)
C1=C2=C3=D1=D2=D3=numeric(0)
for(i in 1:100){
  print(i)
  c = 1:3
  d = 1:3
  for(j in 1:3){
    g = random.graph.game(nodesNum,p[j],directed=FALSE)
    c[j] = is.connected(g)
    d[j] = diameter(g)
  }
  C1 = c(C1,c[1])
  C2 = c(C2,c[2])
  C3 = c(C3,c[3])
  D1 = c(D1,d[1])
  D2 = c(D2,d[2])
  D3 = c(D3,d[3])
}
C1_avg=mean(C1)
C2_avg=mean(C2)
C3_avg=mean(C3)
D1_avg=mean(D1)
D2_avg=mean(D2)
D3_avg=mean(D3)

#(c)
MID=numeric(0)
for(j in 1:100)
{
  print(j)
  left = 0
  right = 1
  flag = F
  step = 0.0001
  mid = (left+right)/2
  while(!isTRUE(flag)){
    mid = (left+right)/2
    gm = random.graph.game(nodesNum,mid,directed=FALSE)
    gl = random.graph.game(nodesNum,(mid-step),directed=FALSE)
    gr = random.graph.game(nodesNum,(mid+step),directed=FALSE)
    if(!isTRUE(is.connected(gl)) && isTRUE(is.connected(gr))){
      flag=T
    }
    else if(isTRUE(is.connected(gm))){
      right = mid
    }
    else {
      left = mid
    }
  }
  MID = c(MID,mid)
}
MID_avg = mean(MID)

cat("precentage of connected when p=0.01: ",C1_avg)
cat("number of connected when p=0.05: ",C2_avg)
cat("number of connected when p=0.1: ",C3_avg)
cat("average diameter when p=0.01: ",D1_avg)
cat("average diameter when p=0.05: ",D2_avg)
cat("average diameter when p=0.1: ",D3_avg)
cat("Pc = ",MID_avg)
