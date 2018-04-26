DyadId <- function(inDT, idcol, neighbour) {
  g <- igraph::graph_from_edgelist(
    as.matrix(unique(inDT[!is.na(get(neighbour)), 
                          .(get(idcol), get(neighbour))])),
    directed = FALSE
  )
  
  simpleG <- igraph::simplify(g)
  
  edgeDT <- data.table(get.edgelist(simpleG),
                       E(simpleG))
  edgeNames <- c('id1', 'id2', 'dyadID')
  setnames(edgeDT, edgeNames)
  
  # Double merge
  # First where left goes to dyadID
  # Then where right goes to dyadID2 and the order revered in by.y
  dyads <- merge(
    merge(inDT, 
          edgeDT,
          by.x = c(idcol, neighbour),
          by.y = edgeNames[1:2],
          all.x = TRUE),
    edgeDT[, .(id1, id2, dyadID2 = dyadID)],
    by.x = c(idcol, neighbour),
    by.y = edgeNames[2:1],
    all.x = TRUE)
  
  # Then dyadID filled with dyadID2
  # and dyadID2 dropped
  dyads[is.na(dyadID), dyadID := dyadID2][, dyadID2 := NULL][]
}