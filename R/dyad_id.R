#' Dyad ID
#'
#' @param DT
#' @param id
#' @param nn
#'
#' @return
#' data.table with first, second ID and dyad ID
#' 
#' @export
#'
#' @examples
dyad_id <- function(DT, id, nn) {
  
  type <- typeof(DT[[id]])
  
  g <- igraph::graph_from_data_frame(unique(na.omit(edges)[, .SD, .SDcols = c(id, nn)]),
                                     directed = FALSE)
  
  simple <- igraph::simplify(g)
  dyads <- data.table::data.table(igraph::get.edgelist(g),
                                  as.numeric(igraph::E(g)))
  nm <- c(id, nn, paste0('dyad', id))
  data.table::setnames(dyads, nm)
  
  dyads[, (c(id, nn)) := lapply(.SD, function(col) as(col, type)),
        .SDcols = c(id, nn)]
  return(dyads)
}

# DT
# nn <- 'NN'
# id <- 'ID'
# 
# g <- igraph::graph_from_data_frame(unique(na.omit(edges)[, .SD, .SDcols = c(id, nn)]),
#                                    directed = FALSE)
# 
# simpleG <- igraph::simplify(g)
# 
# edgeDT <- data.table::data.table(igraph::get.edgelist(simpleG), igraph::E(simpleG))
# edgeNames <- c('id1', 'id2', 'dyadID')
# data.table::setnames(edgeDT, edgeNames)
# 
# # ids <- edgeNames[1:2]
# edgeDT[, (edgeNames) := lapply(.SD, as.numeric)]
# edgeDT
