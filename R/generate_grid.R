#' Generate grid
#' 
#' Generate a regular grid within a polygon object. 
#' 
#' @param pol mcp or other SpatialPolygons object within which the grid will be generated. 
#' @param spacing distance between points in grid. units of the projection.
#' @param crs proj4string of coordinate reference system for pol.
#' @return
#' @export
#'
#' @examples
generate_grid <- function(pol, spacing, crs) {
  rn <- n <- NULL
  
  if (missing(spacing))
    stop('spacing is missing')
  if (missing(crs))
    stop('crs is missing')
  if (missing(pol))
    stop('pol is missing')
  
  extentDT <-
    data.table::data.table(pol@bbox, keep.rownames = TRUE)
  
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
  
  data.table::data.table(rSP@coords, n = 1:length(rSP))[n %in% sp::over(pol, rSP, returnList = TRUE)[[1]]][, n := NULL][]
}
