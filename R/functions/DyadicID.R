library(igraph)

g <- igraph::graph_from_edgelist(
  as.matrix(unique(elk[!is.na(neighbour1), .(id, neighbour1)])),
  directed = FALSE
)

edgeDT <- data.table(get.edgelist(simplify(g)),
                     E(simplify(g)))

setnames(edgeDT, c('id1', 'id2', 'dyadID'))

b <- merge(
  merge(elk, 
        edgeDT,
        by.x = c('id', 'neighbour1'),
        by.y = c('id1', 'id2'),
        all.x = TRUE),
  edgeDT[, .(id1, id2, dyadID2=dyadID)],
  by.x = c('id', 'neighbour1'),
  by.y = c('id2', 'id1'),
  all.x = TRUE)

b[id == 97 | id == 247 | neighbour1 == 97][
  sample(.N, 10), .(id, neighbour1, dyadID, dyadID2)]

b[is.na(dyadID), dyadID := dyadID2]
b[id == 97 | id == 247 | neighbour1 == 97][
  sample(.N, 10), .(id, neighbour1, dyadID, dyadID2)]