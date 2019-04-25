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
  rn <- n <- dif <- NULL
  
  if (missing(spacing))
    stop('spacing is missing')
  if (missing(crs))
    stop('crs is missing')
  if (missing(pol))
    stop('pol is missing')
  
  r <- raster::extent(pol)
  ra <- raster::raster(r)
  raster::res(ra) <- c(spacing, spacing)
  
  raster::projection(ra) <- sp::CRS(crs)
  
  rSP <- raster::rasterToPoints(ra, spatial = TRUE)
  
  data.table::data.table(
    rSP@coords[sp::over(pol, rSP, returnList = TRUE)[[1]], ]
    )
}
