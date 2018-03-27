GenerateGrid <- function(spacing, crs, mcpExtent, DT = NULL, xCol = NULL, yCol = NULL){
  
  extentDT <- data.table(mcpExtent@bbox, keep.rownames = TRUE)
  
  extentDT[, dif := abs(min - max)]  
  
  
  if(extentDT[order(-dif)][1, rn] == "x"){
    xSeq <- extentDT[rn == "x", seq(min, max, by = spacing)]
    ySeq <- extentDT[rn == "y", seq(min, min + (spacing * (length(xSeq) - 1)),
                                    by = spacing)]
  } else {
    ySeq <- extentDT[rn == "y", seq(min, max, by = spacing)]
    xSeq <- extentDT[rn == "x", seq(min, min + (spacing * (length(ySeq) - 1)),
                                    by = spacing)]
  }

  r <- raster::extent(head(xSeq, n = 1), tail(xSeq, n = 1), 
                      head(ySeq, n = 1), tail(ySeq, n = 1))
  ra <- raster(r)
  res(ra) <- c(spacing, spacing)
  
  projection(ra) <- CRS(crs)
  
  rasterToPoints(ra, spatial = TRUE)
  
}
