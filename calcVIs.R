# MSc Thesis
# 15/12/2021
# Calculate vegetation indices

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Link to data and utils
InputLink = "../data/processed/IIASAtrainingFiltered.gpkg"
OutputLink = "../data/processed/IIASAtrainingVIs.gpkg"
source("utils/dataManagement.R")


# Loop over the GPKG layers to extract the bands timeseries
for (band in c(paste0("b", 1:7))){
  
  # Read in band
  InputSF = st_read(InputLink, band)
  
  # Convert SF to DF
  st_geometry(InputSF) = NULL
  InputSF = InputSF[,NewColDates]
  
  # Assign new name
  assign(paste0(band, "_TS"), InputSF)
}


## Calculate Vegetation Indices ##

# NDVI #

# Try to convert values to numeric
sapply(b5_TS, class)
b4Temp = as.data.frame(sapply(b4_TS[,NewColDates], as.numeric))
b5Temp = as.data.frame(sapply(b5_TS[,NewColDates], as.numeric))
sapply(b5Temp, class)

# Calculate NDVI and stats
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)

# Explore NDVI
plot(as.numeric(ndvi[1,]))

# Store NDVI
ndviSF <- DFtoSF(ndvi)
st_write(ndviSF, OutputLink, "NDVI")

