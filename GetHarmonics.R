# MSc Thesis
# 20/12/2021
# Get harmonic and temporal information

# Access to probaV github library
#library(devtools)
#options(unzip = "internal")
#install_github("JornDallinga/probaV")

# Import packages and functions
library(probaV)
library(sf)
source("utils/extractDates.R")
source("utils/harmonicsFunctions.R")

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Link to data
InputLink = "../data/processed/IIASAtrainingVIs.gpkg"
OuputHarmonicsLink = "../data/processed/IIASAtrainingHarmonics.gpkg"

# Get Dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Extract NDVI layer + convert to DF
ndvi = st_read(InputLink, "NDVI")
st_geometry(ndvi) = NULL
ndvi = ndvi[,NewColDates]

# Apply GetHarmMetrics on NDVI timeseries

# Run getHarmonics function and store hamonic metrics
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# Convert HarmMetrics from matrix to df
coordsData = read.csv("../data/processed/IIASAtrainingCoords.csv")
ndvi = cbind(x=coordsData$x, y=coordsData$y, ndvi)
HarmMetrics = cbind(ndvi[, c("x", "y")], HarmMetrics)

# Change colnames
names(HarmMetrics)[3:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save harmonic metrics as gpkg
temp = DFtoSF(HarmMetrics)
st_write(temp, OuputHarmonicsLink, "NDVI")
