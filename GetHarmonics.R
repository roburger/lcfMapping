# MSc Thesis
# 20/12/2021
# Get harmonic and temporal information

# Access to probaV github library
library(devtools)
options(unzip = "internal")
install_github("JornDallinga/probaV")
library(probaV)
library(sf)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Link to data
InputLink = "../data/processed/IIASAtrainingVIs.gpkg"
OuputHarmonicsLink = "../data/processed/IIASAtrainingHarmonics.gpkg"

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Extract NDVI layer + convert to DF
ndvi = st_read(InputLink, "NDVI")
st_geometry(ndvi) = NULL
ndvi = ndvi[,NewColDates]

# Apply GetHarmMetrics on NDVI timeseries
getHarmonics = function(TS){
  
  if (all(is.na(TS))){
    c(min=NA, max=NA, intercept=NA, co=NA, si=NA, co2=NA, si2=NA, trend=NA,
      phase1=NA, amplitude1=NA, phase2=NA, amplitude2=NA)
  }
  
  else {
    HarmCoefs = getHarmMetrics(TS, dates=dates, order=2)
    #HarmCoefs = getHarmMetrics(TS, dates=dates, order=2, return_model = T)
    p1 = phaser(HarmCoefs["co"], HarmCoefs["si"])
    p2 = phaser(HarmCoefs["co2"], HarmCoefs["si2"])
    a1 = amplituder(HarmCoefs["co"], HarmCoefs["si"])
    a2 = amplituder(HarmCoefs["co2"], HarmCoefs["si2"])
    c(HarmCoefs, phase1=p1, amplitude1=a1, phase2=p2, amplitude2=a2)
  }
  #getHarmMetrics(TS, dates=dates, order=2, lin_trend=F)
}

# Phase and Amplitude 
phaser = function(co, si){
  tau = 2*pi
  return(atan2(si, co) %% tau)
}

amplituder = function(co, si){
  return(sqrt(co^2 + si^2))
}

# Run getHarmonics function and store hamonic metrics
HarmMetrics2 = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# Convert HarmMetrics from matrix to df
ndvi = cbind(x=coordsData$x, y=coordsData$y, ndvi)
HarmMetrics = cbind(ndvi[, c("x", "y")], HarmMetrics)

# Change colnames
names(HarmMetrics)[3:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Convert df to sf feature
coords = c("x","y")
tempSF = st_as_sf(HarmMetrics, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"

# Save harmonic metrics as gpkg
st_write(tempSF, OuputHarmonicsLink, "NDVI")
