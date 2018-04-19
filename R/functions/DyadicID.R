pairs <- unique(elk[!is.na(neighbour1), .(id, id2 = neighbour1)])

ids <- unique(c(pairs$id, pairs$id2))

a <- pairs[id %in% c(93, 210, 146)]
a[, pasted := paste0(id, '-', id2)]
a[, tstrsplit(pasted, '-')]

a[, lapply(id2, function(x) grepl(x, pasted)),
  by = .(id)]


combn(ids[1:5], 2)
#



library(igraph)

g <- igraph::graph_from_edgelist(
  as.matrix(unique(elk[!is.na(neighbour1), .(id, neighbour1)])),
  directed = FALSE
)

as.matrix(unique(elk[!is.na(neighbour1), .(id, neighbour1)]))
E(g)
V(g)



data.table(E(g), V(g))

get.edgelist((simplify(g)))
E(simplify(g))
get.edgelist(g)



V(g)$name
get.edgelist(g)
V(g)

edge_attr_names(g)
data.table(V(g))


edge_attr_names(g)
