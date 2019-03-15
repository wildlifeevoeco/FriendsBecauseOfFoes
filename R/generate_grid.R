#' Generate grid
#'
#' @param spacing
#' @param crs
#' @param mcpExtent
#' @param DT
#' @param xCol
#' @param yCol
#'
#' @return
#' @export
#'
#' @examples
generate_grid <-
  function(spacing,
           crs,
           mcpExtent,
           DT = NULL,
           xCol = NULL,
           yCol = NULL) {
    rn <- n <- NULL
    extentDT <- data.table::data.table(mcpExtent@bbox, keep.rownames = TRUE)
    
    extentDT[, dif := abs(min - max)]
    
    
    if (extentDT[order(-dif)][1, rn] == "x") {
      xSeq <- extentDT[rn == "x", seq(min, max, by = spacing)]
      ySeq <-
        extentDT[rn == "y", seq(min, min + (spacing * (length(xSeq) - 1)),
                                by = spacing)]
    } else {
      ySeq <- extentDT[rn == "y", seq(min, max, by = spacing)]
      xSeq <-
        extentDT[rn == "x", seq(min, min + (spacing * (length(ySeq) - 1)),
                                by = spacing)]
    }
    
    r <- raster::extent(head(xSeq, n = 1),
                        tail(xSeq, n = 1),
                        head(ySeq, n = 1),
                        tail(ySeq, n = 1))
    ra <- raster::raster(r)
    raster::res(ra) <- c(spacing, spacing)
    
    raster::projection(ra) <- sp::CRS(crs)
    
    rSP <- raster::rasterToPoints(ra, spatial = TRUE)
    
    data.table::data.table(
      rSP@coords, n = 1:length(rSP))[
        n %in% sp::over(mcpExtent, rSP, returnList = TRUE)[[1]]][, n := NULL]
  }
