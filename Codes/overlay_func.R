# Overlay Function
# ===========================================================================================
ovrly <- function(spdf, pol)
{
  # Overlay
  # -----------------------------------------
  # Find the records that are in side the shape polygon
  pts.in <- spdf[!is.na(overlay(spdf,pol)),]
  dim(pts.in)                            
  cat("points outside the boundary..\n")
  print(nrow(spdf) - nrow(pts.in))         # 12048 points not mapped in cells
  
  # Overlay the points on polygon
  # -----------------------------------------
  o1  <- overlay(spdf,pol)
  o.1 <- o1[!is.na(o1)]                    
  
  # Bind Data
  # -----------------------------------------
  spdf2 <- pts.in
  spdf2@data = cbind(spdf2@data,pol[o.1,])
  print(dim(spdf2))
  spdf.o <<- spdf2
  rm(spdf2)
  gc(verbose =T)
}
# ===========================================================================================