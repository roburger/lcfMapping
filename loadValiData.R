# MSc Thesis
# 19/01/2021
# Load validation data

# Access libraries
library(sf)
library(pbapply)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
ColDates = paste0("X", gsub("-", ".", dates))
ColDates = paste0(NewColDates, "_SR_B1")
  
# Read in validation data (validY)
filename = "../data/raw/refdata_world_africa_included_locations_data20190709.csv"
validationRaw = read.csv(filename, header = T)

#Read in validation data (validX)
# Read in features: NDVI harmonics (trainX)
validationGPKG = st_read("../data/raw/WURValidation2015_Landsat8_TS.gpkg")
st_geometry(validationGPKG)=NULL
# now only reading in first band B1
# should have the same features as training... 
# so calculate harmonics for validation data I guess probably 

all(validationRaw$sample_id %in% validationGPKG$ï..sample_id)
all(validationGPKG$sample_id %in% validationRaw$ï..sample_id)

sapply(validationGPKG, class)
validationGPKG = as.data.frame(sapply(validationGPKG[,NewColDates], as.numeric))
sapply(validationGPKG, class)

plot(as.numeric(apply(validationGPKG, 2, function(x){mean(x, na.rm = TRUE)})))

# Read in b4 and b5 (for ndvi stats)
linkRawValidation = "../data/raw/WURValidation2015_Landsat8_TS.gpkg"
nameBands <- st_layers(linkRawValidation)

b4Validation <- st_read(linkRawValidation, nameBands$name[4])
b5Validation <- st_read(linkRawValidation, nameBands$name[5])
st_geometry(b4Validation)=NULL
st_geometry(b5Validation)=NULL
# save samplID, x and y as df 
temp = data.frame(b4Validation$sample_id, b4Validation$subpix_mean_x, b4Validation$subpix_mean_y)
temp = subset(b4Validation, select = c("sample_id", "subpix_mean_x", "subpix_mean_y"))
write.csv(temp, paste0(linkData, "processed/WURvalidationIDcoords.csv"), row.names=F)

b1Test = validationGPKG
colnames(b1Test)
NewColDates = paste0("X", gsub("-", ".", dates))
colnames(b1Test)= NewColDates

b4Validation = b4Validation[,4:194]
colnames(b4Validation) = NewColDates

# Temporal filter
source("utils/filterBands.R")
b4Filtered = filterBands(b4Validation, smoothLoessPlot, dates)
mean(is.na(b4Filtered)) # 45,5%
mean(is.na(b4Validation)) # 47,4% -> 2% has been filtered

# Apply Filter on other bands
# Read in data per band
b1Validation <- st_read(linkRawValidation, nameBands$name[1])
b2Validation <- st_read(linkRawValidation, nameBands$name[2])
b3Validation <- st_read(linkRawValidation, nameBands$name[3])
b4Validation <- st_read(linkRawValidation, nameBands$name[4])
b5Validation <- st_read(linkRawValidation, nameBands$name[5])
b6Validation <- st_read(linkRawValidation, nameBands$name[6])
b7Validation <- st_read(linkRawValidation, nameBands$name[7])
st_geometry(b1Validation) = NULL
st_geometry(b2Validation) = NULL
st_geometry(b3Validation) = NULL
st_geometry(b4Validation) = NULL
st_geometry(b5Validation) = NULL
st_geometry(b6Validation) = NULL
st_geometry(b7Validation) = NULL
# change column names to make them match
colnames(b1Validation)[4:194] = NewColDates
colnames(b2Validation)[4:194] = NewColDates
colnames(b3Validation)[4:194] = NewColDates
colnames(b4Validation)[4:194] = NewColDates
colnames(b5Validation)[4:194] = NewColDates
colnames(b6Validation)[4:194] = NewColDates
colnames(b7Validation)[4:194] = NewColDates
# Filter on the blue band (most sensitive to clouds)
b2Filtered <- filterBands(b2Validation, smoothLoessPlot, dates)
mean(is.na(b2Validation))
mean(is.na(b2Filtered))

# Apply b2 filter to other bands
b2Matrix = as.matrix(b2Filtered[,NewColDates])

temp = as.matrix(b7Landsat)[,NewColDates]
temp[is.na(b2Matrix)] = NA

b7Filtered[,NewColDates] = temp
mean(is.na(b7Filtered))
mean(is.na(b2Filtered))


for (band in c(paste0("b", 1:7, "Validation"))){
  print(band)

  if (band == "b2Validation"){next}
  
  # Convert to matrix (easy to assign NA's)
  temp = as.matrix(eval(as.symbol(band)))[,NewColDates]
  
  # Assign the b2 filtered NA's to other band
  temp[is.na(b2Matrix)] = NA
  
  # Create copy of b1Validation and assign name b1Filtered
  copy = eval(as.symbol(band))
  copy[,NewColDates] = temp

  assign(gsub("Validation","Filtered", band), copy)
}

# Check if total NA's are the same
mean(is.na(b1Filtered[,NewColDates]))
mean(is.na(b2Filtered[,NewColDates]))
mean(is.na(b3Filtered[,NewColDates]))
mean(is.na(b4Filtered[,NewColDates]))
mean(is.na(b5Filtered[,NewColDates]))
mean(is.na(b6Filtered[,NewColDates]))
mean(is.na(b7Filtered[,NewColDates]))

# Re-add sample ID and coords to b2
temp = subset(b4Validation, select = c("sample_id", "subpix_mean_x", "subpix_mean_y"))
b2Filtered = cbind(temp, b2Filtered)

# Store filtered bands
source("utils/dataManagement.R")
b1FilteredSF <- DFtoSF(b1Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b2FilteredSF <- DFtoSF(b2Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b3FilteredSF <- DFtoSF(b3Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b4FilteredSF <- DFtoSF(b4Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b5FilteredSF <- DFtoSF(b5Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b6FilteredSF <- DFtoSF(b6Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)
b7FilteredSF <- DFtoSF(b7Filtered, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE)

# Save as one gpkd with mulitple layers
st_write(b1FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"processed/WURvalidationFiltered.gpkg"), "b7")

## Calc VI ##
# Read in data (filtered bands)
b4Filtered = st_read("../data/processed/WURvalidationFiltered.gpkg", "b4")
b5Filtered = st_read("../data/processed/WURvalidationFiltered.gpkg", "b5")
coordsID = read.csv("../data/processed/WURvalidationIDcoords.csv")
st_geometry(b4Filtered)=NULL
st_geometry(b5Filtered)=NULL

# Convert to numeric
b4Filtered = as.data.frame(sapply(b4Filtered[,NewColDates], as.numeric))
b5Filtered = as.data.frame(sapply(b5Filtered[,NewColDates], as.numeric))

# Calculate
ndvi = (b5Filtered - b4Filtered) / (b5Filtered + b4Filtered)
plot(as.numeric(apply(ndvi, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndvi")

# Save ndvi as gpkg
temp = cbind(coordsID,ndvi)
ndviSF <- DFtoSF(temp, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE) # first source
st_write(ndviSF, "../data/processed/WURvalidationVIs.gpkg", "NDVI")

## Get ndvi harmonics ##
# Link to libraries
library(devtools)
library(probaV)
# Link to data
InputLink = "../data/processed/WURvalidationVIs.gpkg.gpkg"
OuputHarmonicsLink = "../data/processed/WURvalidationHarmonics.gpkg"

# LOAD FUNCTIONS FROM GETHARMONICS.R AMPLITUDER, PHASER, GETHARMONICS

# Apply function to get the harmonics of NDVI
test = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(coordsID[, c("sample_id","subpix_mean_x", "subpix_mean_y")], test)

# Change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save harmonics 
HarmMetricsSF <- DFtoSF(HarmMetrics, coords = c("subpix_mean_x","subpix_mean_y"), validation = TRUE) # first source
st_write(HarmMetricsSF, OuputHarmonicsLink, "NDVI")
