library(igraph)
DyadId <- function(DT, id, neighbour) {
  g <- igraph::graph_from_edgelist(
    as.matrix(unique(DT[!is.na(get(neighbour)), .(id, get(neighbour))])),
    directed = FALSE
  )
  
  edgeDT <- data.table(get.edgelist(simplify(g)),
                       E(simplify(g)))
  edgeNames <- c('id1', 'id2', 'dyadID')
  setnames(edgeDT, edgeNames)

  # Double merge
  # First where left goes to dyadID
  # Then where right goes to dyadID2 and the order revered in by.y
  dyads <- merge(
    merge(DT, 
          edgeDT,
          by.x = c(id, neighbour),
          by.y = edgeNames,
          all.x = TRUE),
    edgeDT[, .(id1, id2, dyadID2 = dyadID)],
    by.x = c(id, neighbour),
    by.y = edgeNames[2:1],
    all.x = TRUE)
  
  # Then dyadID filled with dyadID2
  # and dyadID2 dropped
  dyads[is.na(dyadID), dyadID := dyadID2][, dyadID2 := NULL]
}