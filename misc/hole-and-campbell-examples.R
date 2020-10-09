library(igraph)

x <- read.table('table-5.2.txt', sep = ',')
str(x)
x

nm <- c('An', 'Bk', 'Br', 'Dm', 'Dw', 'La', 'Ld', 'Me', 'Mp', 'Pa', 'Re', 'Sk', 'Sv', 'Vn', 'Kb', 'El', 'Mn', 'Em')

x <- as.matrix(x)
dimnames(x) <- list(nm, nm)

x

g <- graph_from_adjacency_matrix(x, mode = 'upper', diag = FALSE, weighted = TRUE)
cm <- cluster_walktrap(g)

par(mar = c(0,0,0,0))
plot(g)

plot(g, vertex.size = sqrt(degree(g) * 25), vertex.label.family = 'sans')


plot(cm, g, vertex.label.family = 'sans')
