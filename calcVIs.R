# MSc Thesis
# Start: 15/12/2021
# Modified: 28/03/2022
# Finalised: 31/05/2022
# Calculate vegetation indices

# Set working directory (for yourself)
setwd("~/Thesis/code/lcfMapping/")

# Set libraries
library(sf)
source("utils/dataManagement.R")
source("utils/extractDates.R")

# Link to data and utils
InputLink = "../data/processed/IIASAtrainingFiltered.gpkg"
OutputLink = "../data/processed/IIASAtrainingVIs.gpkg"

# Get Dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Loop over the GPKG layers to extract the bands timeseries
for (band in c(paste0("b", 1:7))){
  
  # Read in band
  InputSF = st_read(InputLink, band)
  
  # Convert SF to DF
  st_geometry(InputSF) = NULL
  #InputSF = InputSF[,NewColDates]
  
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

# Read NDVI
ndvi = st_read(OutputLink, "NDVI")
st_geometry(ndvi) = NULL
ndviComplete = ndvi
ndvi = ndvi[,NewColDates]



# Unused features below (not used further):
# NDVI stats
head(as.numeric(apply(ndvi, 1, mean, na.rm=T)), 5)
ndvi[1,]
mean(as.numeric(ndvi[3,]), na.rm=T)
IQR(as.numeric(ndvi[3,]), na.rm=T)

temp = as.numeric(apply(ndvi, 1, IQR, na.rm=T))
iqr = data.frame(IQR=temp)
temp = as.numeric(apply(ndvi, 1, median, na.rm=T))
median = data.frame(medianNDVI=temp)
