# Geocoding script for large list of addresses.
#load up the ggmap library
library(ggmap)
# Two inputs
# Data Preparation: Crime csv and Shapefile Data
# ====================================================================================
sys <- "//hkgrhekubd001/Production/"
setwd(paste0(sys,"Parth/Serene Share/Project/CrimeMapsv2.0/Data"))

# Year Wise Data from Gov.in
# ----------------------------------
crm13 <- read.csv("Raw/Crime Data/dstrIPC_2013.csv")
dim(crm13)
crm12 <- read.csv("Raw/Crime Data/dstrIPC_1.csv")
dim(crm12)
table(names(crm13) %in% names(crm12))

# Consolidate
# ----------------------------------
crm <- rbind(crm12,crm13)
dim(crm)
crm$add <- as.character(paste0(crm$DISTRICT,",",crm$STATE.UT))
length(unique(crm$DISTRICT))
pth <- "//hkgrhekubd001/Production/Parth/Serene Share/Project/CrimeMapsv2.0/Data/Raw/Batch Geo-code/"
write.csv(crm, paste0(pth, "Batch_crime.csv"))
cr <- subset(crm, DISTRICT != "TOTAL")
crt <-subset(cr, YEAR == "2001")

# Consolidate
# ----------------------------------
infile <- "Batch_crime"
data <- read.csv(paste0(pth, infile, '.csv'))
dim(data)
# ====================================================================================
# ====================================================================================

# get the address list, and append "Ireland" to the end to increase accuracy 
# (change or remove this if your address already include a country etc.)
addresses = unique(data$add)
addresses = paste0(addresses, ", India")


#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}


#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}

# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}

#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$lat
data$accuracy <- geocoded$accuracy

#finally write it all to the output files
saveRDS(data, paste0(pth, infile ,"_geocoded.rds"))
write.table(data, file=paste0(pth, infile ,"_geocoded.csv"), sep=",", row.names=FALSE)