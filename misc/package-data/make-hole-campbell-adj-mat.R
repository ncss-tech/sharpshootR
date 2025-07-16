library(igraph)

# table 5.2 from
# Hole, F.D. and J.B. Campbell. Soil Landscape Analysis. Rowman Allanheld, 1985.

# hand-keyed
x <- read.table('table-5.2.txt', sep = ',')

# check: OK
str(x)
x

nm <- c('An', 'Bk', 'Br', 'Dm', 'Dw', 'La', 'Ld', 'Me', 'Mp', 'Pa', 'Re', 'Sk', 'Sv', 'Vn', 'Kb', 'El', 'Mn', 'Em')

# DF -> matrix
x <- as.matrix(x)
dimnames(x) <- list(nm, nm)

# check: OK
x

# note special incantation to get the "correct" graph structure
g <- graph_from_adjacency_matrix(x, mode = 'upper', diag = FALSE, weighted = TRUE)

# does it make sense?
# seems to
par(mar = c(0,0,0,0))
plot(g)

plot(g, vertex.size = sqrt(igraph::degree(g) * 25), vertex.label.family = 'sans')

# find communities
cm <- igraph::cluster_walktrap(g)
plot(cm, g, vertex.label.family = 'sans')

# rename / save
table5.2 <- x
save(table5.2, file = '../data/table5.2.rda')
