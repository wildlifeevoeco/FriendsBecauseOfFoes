GenerateGrid <- function(spacing, mcpExtent, DT = NULL, xCol = NULL, yCol = NULL){
  
  extentDT <- data.table(wolfMCP@bbox, keep.rownames = TRUE)
  
  extentDT[, dif := abs(min - max)]  
  
  if(extentDT[order(-dif)][1, rn] == "x"){
    xSeq <- extentDT[rn == "x", seq(min, max, by = spacing)]
    ySeq <- extentDT[rn == "y", seq(min, min + (spacing * (length(xSeq) - 1)),
                                    by = spacing)]
    data.table(x = xSeq, y = ySeq, rowID = 1:length(xSeq))
  } else {
    ySeq <- extentDT[rn == "y", seq(min, max, by = spacing)]
    xSeq <- extentDT[rn == "x", seq(min, min + (spacing * (length(ySeq) - 1)),
                                    by = spacing)]
    data.table(x = xSeq, y = sySeq, rowID = 1:length(xSeq))
  }
}
