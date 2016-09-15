library("igraph")
library("netrw")
#(a)
N=1000
p=0.01
g=random.graph.game(N,p,directed=TRUE)

#random walk
r = netrw(g, damping=0.85,T=1000, output.walk.path=TRUE,output.visit.prob=TRUE)
rw <- netrw(g,walker.num,start.node=1:vcount(g),
            damping=0.85,weights=NULL,T=100,seed=NULL,
            output.walk.path=TRUE,output.walkers=0:(walker.num-1),
            output.visit.prob=TRUE,output.nodes=0:(vcount(g)-1),
            output.device="memory",
            local.pagerank=FALSE,teleport.prob=NULL)
pagerank=rw$ave.visit.prob
plot(pagerank, main = "Default PageRank",pch=1)

#(b)
pageRank <- page.rank(g,directed=TRUE,damping=0.85)
r_my = netrw(g,walker.num,start.node=1:vcount(g),
             damping=0.85,weights=NULL,T=step,seed=NULL,
             output.walk.path=TRUE,output.walkers=0:(walker.num-1),
             output.visit.prob=TRUE,output.nodes=0:(vcount(g)-1),
             output.device="memory",
             local.pagerank=TRUE,teleport.prob=pageRank$vector)
plot(r_my$ave.visit.prob,main="Personalized PageRank",xlab="Node",ylab="rankscore")


#(c)
N_pr = rep(1/N,N)
r_default = netrw(g,walker.num,start.node=1:vcount(g),
                  damping=0.85,weights=NULL,T=step,seed=NULL,
                  output.walk.path=TRUE,output.walkers=0:(walker.num-1),
                  output.visit.prob=TRUE,output.nodes=0:(vcount(g)-1),
                  output.device="memory",
                  local.pagerank=TRUE,teleport.prob=N_pr)
plot(r_default$ave.visit.prob, main = "Regular PageRank")
