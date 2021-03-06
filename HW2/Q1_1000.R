library ("igraph")
library ("netrw")
p <- 0.01
node_num <- 1000

gra <- random.graph.game(node_num, p, directed = F)
dia <- diameter(gra)
deg <- degree.distribution(gra)
plot(deg, xlab = "degree", ylab = "frequency" ,type = "o")

step <- 100
walker_num <- node_num
ave <- rep(NA,step)
sdv <- rep(NA,step)
for (t in (1:step)) {
    rw <- netrw(gra,walker.num = walker_num, start.node = 1:vcount(gra),
                T = t, output.walk.path = TRUE, output.walkers = 0:(walker_num-1))
    shortest_path <- rep(NA, walker_num)
    for(w in (1:walker_num)) {
        temp <- get.shortest.paths(gra, from = rw$walk.path[1,w], to = rw$walk.path[t,w])
        shortest_path[w] <- length(temp$vpath[[1]])-1
    }
    ave[t] <- mean(shortest_path)
    sdv[t] <- sd(shortest_path)
}

plot(1:step, ave, type = "o", xlab = "step", ylab = "average distance")
plot(1:step, sdv, type = "o", xlab = "step", ylab = "standard deviation")

end_degree <- rep(NA, walker_num)
for(w in (1:walker_num)) {
    end_degree[w] <- degree(gra, rw$walk.path[step,w])
}
hist(end_degree, breaks = seq(-0.5, by = 1, length.out = max(end_degree)+2), xlab = "degree", ylab = "frequency")


