# Prilimnaries
# ====================================================================================
rm(list=ls())
gc(verbose=T)
gcinfo(FALSE)
sys <- "/Users/parthkhare/Work/Data Mining/"
setwd(paste0(sys, "CrimeMapsv2.0/Data"))

load("nwt_testing_subset.RData")
load("nwt_locations.RData")
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
library(RColorBrewer)
# ====================================================================================


# Crime Data Fitting
# ====================================================================================
# Data I (locs)
# -----------------------------
dt <- readShapeSpatial(paste0(sys, "CrimeMapsv2.0/Data/Raw/State_Shapefiles/AllIndiaStatesV4withCentroids.shp"))
locs <- as.data.frame(dt)
locs$OBJECTID <- NULL
locs$IS_SC <- NULL
locx <- locs
names(locx)[names(locx) == "S_Name"] <- "loc"
names(locx)[names(locx) == "Lat"] <- "lat"
names(locx)[names(locx) == "Lon"] <- "lon"
locx$loc <- as.character(locx$loc)
locs <- locx

# Data Primers
# -----------------------------
decades <- seq(2001, 2013, by=1)
lon <- 78.7378
lat <- 23.8388
d <- d.cru$Locs[[2]] %>% filter(Month=="Jan")

# Simulate Multiple Layers to Stack
# -----------------------------
pth <- paste0(sys, "CrimeMapsv2.0/Data/Processed/IPC versions/")
sim <- paste0(pth, "IPC_v2/")
st1 <- raster(paste0(sim, "stIPC_2001.tif"))
st2 <- raster(paste0(sim, "stIPC_2002.tif"))
st3 <- raster(paste0(sim, "stIPC_2003.tif"))
st4 <- raster(paste0(sim, "stIPC_2004.tif"))
st5 <- raster(paste0(sim, "stIPC_2005.tif"))
st6 <- raster(paste0(sim, "stIPC_2006.tif"))
st7 <- raster(paste0(sim, "stIPC_2007.tif"))
st8 <- raster(paste0(sim, "stIPC_2008.tif"))
st9 <- raster(paste0(sim, "stIPC_2009.tif"))
st10 <- raster(paste0(sim, "stIPC_2010.tif"))
st11 <- raster(paste0(sim, "stIPC_2011.tif"))
st12 <- raster(paste0(sim, "stIPC_2012.tif"))
st13 <- raster(paste0(sim, "stIPC_2013.tif"))

# Simulate Multiple Layers to Stack
# -----------------------------
names(st1)[names(st1) == "stIPC_2001"] <- "index_2001"
names(st2)[names(st2) == "stIPC_2002"] <- "index_2002"
names(st3)[names(st3) == "stIPC_2003"] <- "index_2003"
names(st4)[names(st4) == "stIPC_2004"] <- "index_2004"
names(st5)[names(st5) == "stIPC_2005"] <- "index_2005"
names(st6)[names(st6) == "stIPC_2006"] <- "index_2006"
names(st7)[names(st7) == "stIPC_2007"] <- "index_2007"
names(st8)[names(st8) == "stIPC_2007"] <- "index_2008"
names(st9)[names(st9) == "stIPC_2009"] <- "index_2009"
names(st10)[names(st10) == "stIPC_2010"] <- "index_2010"
names(st11)[names(st11) == "stIPC_2011"] <- "index_2011"
names(st12)[names(st12) == "stIPC_2012"] <- "index_2012"
names(st13)[names(st13) == "stIPC_2013"] <- "index_2013"


# Layer Stack
# -----------------------------
stck <- stack(st1,st2,st3,st4,st5,st6,st7,st8,st9,st10,st11,st12,st13)
class(stck)
br.pc <- brick(stck)
nlayers(br.pc)
names(br.pc)
class(br.pc)
# View(br.pc@data@values)
x <- br.pc
names(x)

# From Rasterbrick
# -----------------------------
# pth2 <- paste0(sys, "Parth/Serene Share/Project/CrimeMapsv2.0/Data/")
# load(paste0(pth2, "Processed/Crime_raster/ipc_brick.RData"))
# class(br)
# nlayers(br)
# y <- dropLayer(br, 10:14)
# class(y)
# y <- brick(y)
# class(y)
# names(y) <- names(br.sim)
# x <- y
# names(x)
nlayers(br.sim)
nlayers(y)

# # ====================================================================================
# Saving Image
save(x,locx,lat,lon,decades, file = "App/IPC_3.RData")
save(x,locx,lat,lon,decades, file = "/Users/parthkhare/Work/Data Mining/CrimeMapsv2.0/Sessions/SociocaRtography/IPC_3.RData")
# # Mac Transfer
# save.image(file = paste0(pth2,"Processed/bricks_26jun.RData"))
# load(paste0(pth2,"Processed/bricks_26jun.RData"))
# # ====================================================================================

