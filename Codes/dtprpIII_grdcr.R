# Prilimnaries
# ====================================================================================
rm(list=ls())
gc(verbose=T)
gcinfo(FALSE)
sys <- "/Users/parthkhare/Work/Data Mining/"
setwd(paste0(sys, "CrimeMapsv2.0/Sessions/IPC"))
# ====================================================================================


# Libraries
# ====================================================================================
library(maps)
library(mapproj)
library(sp)
library(maptools)
library(raster)
library(leaflet)
library(shiny)
library(ggplot2)
library(shinyBS)
library(rgdal)
library(dplyr)
library(data.table)
# ====================================================================================

# Data Import
# ====================================================================================
# Import Night Light tiff
# ---------------------------
ras <- readGDAL(paste0(sys, "CrimeMapsv2.0/Data/Raw/Night Lights/rad2000resam1_V1.tif"))
# ras <- raster(paste0(sys, "Parth/Serene Share/Project/CrimeMapsv2.0/Data/Raw/Night Lights/rad2000resam1_V1.tif"))
ras <- raster(ras)
class(ras)
extent(ras)
#ras = Nighlights 1 km

# Import Crime Data on District Polygons
# ---------------------------
pth2 <- paste0(sys, "CrimeMapsv2.0/Data/")
cd <- readShapeSpatial(paste0(pth2, "Processed/Crime_shp/MetaCrime_pol.shp"))
class(cd)
names(cd)
# ====================================================================================

# Crime Meta Data Calculations
# ====================================================================================
# Subset for one variable
# --------------------------------
v <- c("S_Nam","D_Nam")
v1 <- names(cd)[grepl("IPC_20", names(cd))]
voi <- c(v, v1)
ipc <- cd[,voi]
names(ipc)
class(ipc)

# Checking Data consistency
# --------------------------------
sd.cols <- names(ipc)[grepl("IPC_2", names(ipc))]
dtt <- data.table(ipc@data)
dtt[,lapply(.SD, summary),.SDcols = sd.cols]

# NA Treatment
# --------------------------------
ip2 <- ipc
ip2 <- na.omit(ip2)

# Outlier Treatment
# --------------------------------
# Outlier Study
for(i in 3:15)
{
  print(quantile(ip2@data[[i]], p = c(0.01,0.99,0.995,0.999,1), na.rm =T))
  plot(density(ip2@data[[i]],na.rm=T))
}
# Right Tail Outlier Capping @ 99%
for(i in 3:15)
{
  ip2@data[[i]] <- ifelse(ip2@data[[i]] >= quantile(ip2@data[[i]], p =1, na.rm=T),
                          quantile(ip2@data[[i]], p = 0.99, na.rm=T), ip2@data[[i]])
}
# Left Tail Outlier Capping @ 99%
# for(i in 3:15)
# {
#   ip2@data[[i]] <- ifelse(ip2@data[[i]] >= quantile(ip2@data[[i]], p =1, na.rm=T),
#                           quantile(ip2@data[[i]], p = 0.99, na.rm=T), ip2@data[[i]])
# }
# Check
for(i in 3:15)
{
  print(quantile(ip2@data[[i]], p = c(0.75,0.99,0.995,0.999,1),na.rm=T))
  plot(density(ip2@data[[i]],na.rm=T))
}

# Specific Case of 2013
# --------------------------------
quantile(ip2@data$IPC_2013, p = c(0.979,0.98,0.981,0.985,0.986,0.988,0.999,1),na.rm=T)
ip2@data$IPC_2013 <- ifelse(ip2@data$IPC_2013 >= quantile(ip2@data$IPC_2013, 
                                                          p = 0.975, na.rm=T),
                            quantile(ip2@data$IPC_2013, p = 0.975, na.rm=T), 
                            ip2@data$IPC_2013)
quantile(ip2@data$IPC_2013, p = c(0.979,0.98,0.981,0.985,0.986,0.988,0.999,1),na.rm=T)

# Normalizing and Extreme Value Treatment
# --------------------------------
for(i in 3:15)
{
  ip2@data[[i]] <- scale(ip2@data[[i]])
  print(quantile(ip2@data[[i]], p = c(0.75,0.99,0.995,0.999,1),na.rm=T))
}


# Treatment Variable
ipc <- ip2
# rm(ip2)
# ====================================================================================



# GRID Calculations
# ====================================================================================
# Extent Computations
#---------------------------
x_min <- extent(ras)[1]
x_max <- extent(ras)[2]
y_min <- extent(ras)[3]
y_max <- extent(ras)[4]
cell_res <- 0.08333333  # 10 km GRID
#---------------------------

# Number of Columns & Rows : Computations
#--------------------------
x_extent <-as.integer((x_max-x_min)/cell_res)
x_extent.1 <- (((x_max-x_min)/cell_res)-as.integer((x_max-x_min)/cell_res))  #Long
y_extent <- as.integer((y_max-y_min)/cell_res)
y_extent.1 <- (((y_max-y_min)/cell_res)-as.integer((y_max-y_min)/cell_res))  #Lat
n_row <- ifelse(y_extent.1>0.5,(y_extent+2),(y_extent+1))    #lat
n_col <- ifelse(x_extent.1>0.5,(x_extent+2),(x_extent+1))    #long

# Empty Raster
#--------------------------
ras1 <- raster(nrow=n_row,ncol=n_col)
extent(ras1) <- extent(ras)
# ====================================================================================

# Creating Rasterized District Polygon Data
# ====================================================================================
# Resampling from 1km to 10km
#--------------------------
ras2 <- resample(ras,ras1,method='bilinear')
extent(ras2) <- extent(cd)

# Rasterize
#--------------------------
pth <- paste0(sys, "CrimeMapsv2.0/Data/Processed/IPC versions/")
# sim <- paste0(pth, "IPC_v1/")
sim <- paste0(pth, "IPC_v2/")
r3 <- rasterize(ipc, ras2, "IPC_2001", update=TRUE, CRS("+init=epsg:4326"),fun=sum)
writeRaster(r3,file.path(sim,"stIPC_2001"),format="GTiff",overwrite=TRUE)
r4 <<- r3

# Outlier Treatment 2013
# --------------------------------
# r2013 <- rasterize(ipc, ras2, "IPC_2013", update=TRUE, CRS("+init=epsg:4326"),fun=sum)
# writeRaster(r2013,file.path(sim,"stIPC_2013"),format="GTiff",overwrite=TRUE)

# Variable for looping: Export individual raster to stack them
# --------------------------------
library(Hmisc)
v2 <- v1[v1 %nin% c("IPC_2001")]
t1 <- Sys.time()
for(i in v2 )
{
  print(i)
  rar <- rasterize(ipc, ras2,i, update=TRUE, CRS("+init=epsg:4326"),fun=sum)
  print(summary(rar))
  flnm <- paste0("st", i)
  print(flnm)
  writeRaster(rar,file.path(sim,flnm),format="GTiff",overwrite=TRUE)
  r4 <<- stack(r4,rar)
  cat("\n")
}
t2 <- Sys.time()- t1

# Brick from Stack
# --------------------------------
nlayers(r4)
br.pc <- brick(r4,values=TRUE)
nlayers(br.pc)
names(br.pc)
class(br.pc)
View(br.pc@data@values)
x <- br.pc
names(x)

# Export Raster Brick
# --------------------------------
# writeRaster(br,file.path(pth,"Brick_10kmv1"),format="GTiff",overwrite=TRUE)
# r5 <- unstack(r4)
# outputnames <- paste(seq_along(r5), ".nc",sep="")
# for(i in seq_along(r5)){writeRaster(r5[[i]], file=outputnames[i])}
#                                     file.path(pth,"Brick_10kmv1"),format="GTiff",overwrite=TRUE)}
# 
# 
# rasterfiles   <- list.files("C:/", "*.envi", full.names = TRUE)
# d1 <-  overlay(stack(rasterfiles ), 
#                fun=function(x) movingFun(x, fun=mean, n=3, na.rm=TRUE), 
#                filename='output.tif' )
# 
# # Export Raster Brick
# # --------------------------------
# bsd <- raster(paste0(pth, "Brick_10kmv1.tif"))
# nlayers(bsd)
# ====================================================================================
# FIN