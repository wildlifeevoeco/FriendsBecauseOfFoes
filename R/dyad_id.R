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
  
  g <- igraph::graph_from_data_frame(
    unique(na.omit(edges)[, .SD, .SDcols = c(id, nn)]),
    directed = FALSE
  )
  
  simple <- igraph::simplify(g)
  
  dyads <- data.table::data.table(
    igraph::get.edgelist(simple),
    dyadID = as.numeric(igraph::E(simple))
  )

  
  dyads <- rbindlist(
    list(dyads[, .(V1, V2, dyadID)],
         dyads[, .(V1 = V2, V2 = V1, dyadID)])
    )

  nm <- c(id, nn, paste0('dyad', id))
  data.table::setnames(dyads, nm)

  dyads[, (c(id, nn)) := lapply(.SD, function(col) as(col, type)),
        .SDcols = c(id, nn)]

  return(dyads[])
}