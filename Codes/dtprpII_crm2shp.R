# Crime Maps
# ====================================================================================
rm(list = ls())

# Libraries
# ====================================================================================
library(maptools)
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(leaflet)
library(shiny)
library(plyr)
library(Hmisc)
library(ggplot2)
library(googleVis)
library(ggmap)
# ====================================================================================

#load up the ggmap library


# Two inputs
# Data Preparation: Crime csv and Shapefile Data
# ====================================================================================
sys <- "/Users/parthkhare/Work/Data Mining/"
setwd(paste0(sys,"CrimeMapsv2.0/Data"))
pth <- paste0(sys, "CrimeMapsv2.0/Data/Raw/Batch Geo-code/")

# Associate Addresses with main data 
# ====================================================================================
# Read Geocoded Crime Data
# ------------------------------------
infile <- "Batch_crimev2"
data <- read.csv(paste0(pth, infile ,"_geocoded.csv"))
dim(data)
names(data)
table(is.na(data$lat))

# Create SPDF
# --------------------------------
coords <- cbind(data$long, data$lat)   # x1: long; x2: lat
spdf <- SpatialPointsDataFrame(coords, data)
class(spdf)
dim(spdf)                         
proj4string(spdf) <- "+proj=longlat +datum=WGS84"

# Import District Shapefile
# --------------------------------
dst <- readShapeSpatial("Raw/District_Shapefiles/AllIndiaDistrictsV5.shp")
dim(dst)

# Merge crime data with geocoded lat-longs
# --------------------------------
source("/Users/parthkhare/Work/Data Mining/CrimeMapsv2.0/Codes/overlay_func.R")
system.time(ovrly(spdf= spdf, pol = dst))
crm.dst <- spdf.o
dim(crm.dst)
rm(spdf.o)

# Aggregate multiple records for a district [currently the data is at address level]
# --------------------------------
crm.dst$ID <- paste0(crm.dst$D_Name, ",", crm.dst$S_Name)
crm.dst@data$X <- NULL
crm.dst@data$add <- NULL
crm.dst@data$IS_SC <- NULL
crm.dst@data$ID <- NULL
crdt <- data.table(crm.dst@data)

# Aggregation Variables NameSubSetting
ves <- c("lat", "long", "S_Name", "D_Name", "ID")
library(Hmisc)
voi <- names(crdt)[names(crdt) %nin% ves]

# Aggregation 
sd.cols = voi
cdt <- crdt[,lapply(.SD, sum, na.rm =T), .SDcols = sd.cols, by = .(IS_DC)]  # simple
dim(cdt)

# Checking Data consistency
# ----------------------
cdt[,lapply(.SD, summary),.SDcols = sd.cols]

# Intersect for final shapefile
# --------------------------------
cdt2 <- data.frame(cdt)
cd <- merge(x = dst, y  = cdt2, by = "IS_DC")
class(cd)
dim(cd)

# Refine Names
# ----------------------------------
names(cd) <- gsub("TOTAL_IPC_CRIMES", "IPC",names(cd))
names(cd)

# Checking Data consistency
# ----------------------
v1 <- names(cd)[grepl("IPC.", names(cd))]
dtt <- data.table(cd@data)
sd.cols = v1
dtt[,lapply(.SD, summary),.SDcols = sd.cols]

# Export Data into Shapefile
# ----------------------------------
proj4string(cd) <- CRS("+proj=longlat +datum=WGS84")
writeOGR(cd, "Processed/Crime_shp","MetaCrime_pol",
         driver="ESRI Shapefile", overwrite = T)
# ====================================================================================
