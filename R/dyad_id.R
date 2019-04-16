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
  g <- igraph::graph_from_data_frame(unique(na.omit(edges)[, .SD, .SDcols = c(id, nn)]),
                                     directed = FALSE)
  
  dyads <- data.table::data.table(igraph::get.edgelist(g),
                                  igraph::E(g))
  nm <- c(paste0(id, c(1, 2)), paste0('dyad', id))
  data.table::setnames(dyads, nm)
  
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
