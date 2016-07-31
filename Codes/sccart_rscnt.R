library(rsconnect)
rsconnect::setAccountInfo(name='sociocartography',
                          token='26BB882140906240DE4D5B8088D15453',
                          secret='pY0rs6m97iBaeUoc8SdlWy89bdhXgjeyKBYHjzDj')

# sys <- "/Users/parthkhare/Work/Data Mining/"
# setwd(paste0(sys, "CrimeMapsv2.0/Codes/SessionIPC"))
rm(list=ls())
gc(verbose=T)
deployApp()
