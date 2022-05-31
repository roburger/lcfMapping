# MSc Thesis
# Apply temporal filter
# Start: 13/12/2021
# Modified: 28/03/2022
# Finalized: 31/05/2022

# Set working directory
# adjust for yourself !
setwd("~/Thesis/code/lcfMapping/")

# Import packages
library(sf)
library(dplyr)
library(pbapply)
source("utils/extractDates.R")
source("utils/filterBands.R")
source("utils/dataManagement.R")


# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/" # adjust to own folder !

# Retrieve band layers
linkLandsatIIASA2015 <- paste0(linkData, "raw/IIASATraining2015_Landsat8_TS.gpkg")
nameBands <- st_layers(linkLandsatIIASA2015)

# Read in data per band
b1Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[1])
b2Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[2])
b3Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[3])
b4Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[4])
b5Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[5])
b6Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[6])
b7Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[7])

# Remove geometries
st_geometry(b1Landsat) = NULL
st_geometry(b2Landsat) = NULL
st_geometry(b3Landsat) = NULL
st_geometry(b4Landsat) = NULL
st_geometry(b5Landsat) = NULL
st_geometry(b6Landsat) = NULL
st_geometry(b7Landsat) = NULL

# Get Dates
dates = extractDates()
ColDates = paste0("X", gsub("-", ".", dates), "_SR_B1")
NewColDates = paste0("X", gsub("-", ".", dates))

# change column names to make them match with other bands
colnames(b1Landsat)[4:194] = NewColDates
colnames(b2Landsat)[4:194] = NewColDates
colnames(b3Landsat)[4:194] = NewColDates
colnames(b4Landsat)[4:194] = NewColDates
colnames(b5Landsat)[4:194] = NewColDates
colnames(b6Landsat)[4:194] = NewColDates
colnames(b7Landsat)[4:194] = NewColDates

# Get XY coords
DFcoords = b1Landsat[c("x","y")]
write.csv(DFcoords, paste0(linkData, "processed/IIASAtrainingCoords.csv"), row.names=F)

## TEST and EXPERIMENT BELOW
# Apply filter to remove outliers
b1Filtered <- filterBands(b1Landsat[1:1000,], smoothLoessPlot, dates)
mean(is.na(b1Filtered))
mean(is.na(b1Landsat[1:1000,NewColDates]))

# st_write and write.csv test
b1FilterTest = b1Filtered
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))
b1FilterTest = cbind(b1FilterTest, x=coordsData$x, y=coordsData$y)
coords = c("x","y")
tempSF = st_as_sf(b1FilterTest, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"
st_write(tempSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.gpkg")
write.csv(b1Filtered, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.csv")



# APPLY FILTER ON BAND 2 (BLUE)
b2Filtered <- filterBands(b2Landsat, smoothLoessPlot, dates) # takes some time...
mean(is.na(b2Filtered))
mean(is.na(b2Landsat[,NewColDates]))

# Write and store Filtered band as SF
b2FilteredSF <- DFtoSF(b2Filtered)
st_write(b2FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b2Filtered.gpkg")

# Remove new NAs for all bands
# Now done for the band itself...

# Apply b2 filter to all bands
b1Filtered = applyFilter(b1Landsat, b2Filtered)

mean(is.na(b2Filtered[,NewColDates])) # 45% NA
mean(is.na(b1Landsat[,NewColDates])) # 43% NA
mean(is.na(b1Filtered[,NewColDates])) # 45% NA

b3Filtered = applyFilter(b3Landsat, b2Filtered)
b4Filtered = applyFilter(b4Landsat, b2Filtered)
b5Filtered = applyFilter(b5Landsat, b2Filtered)
b6Filtered = applyFilter(b6Landsat, b2Filtered)
b7Filtered = applyFilter(b7Landsat, b2Filtered)

# Change to SF format
b1FilteredSF <- DFtoSF(b1Filtered)
b2FilteredSF <- DFtoSF(b2Filtered)
b3FilteredSF <- DFtoSF(b3Filtered)
b4FilteredSF <- DFtoSF(b4Filtered)
b5FilteredSF <- DFtoSF(b5Filtered)
b6FilteredSF <- DFtoSF(b6Filtered)
b7FilteredSF <- DFtoSF(b7Filtered)

# Save as gpkg (better to do it below)
# st_write(b1FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.gpkg")
# st_write(b2FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b2Filtered.gpkg")
# st_write(b3FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b3Filtered.gpkg")
# st_write(b4FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b4Filtered.gpkg")
# st_write(b5FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b5Filtered.gpkg")
# st_write(b6FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b6Filtered.gpkg")
# st_write(b7FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b7Filtered.gpkg")

# Save as one gpkg with multiple layers
st_write(b1FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"processed/IIASAtrainingFiltered.gpkg"), "b7")

# test multiple layers
st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b2")




## Temporal filter Function ##
# now stored in utils/filterBands.R

smoothLoessPlot = function (tsx, QC_good = NULL, dates = NULL, threshold = c(-50, Inf),
                            res_type = c("distance", "sd_distance", "all", "filled","omit", "QC"),
                            span=0.3, family="gaussian", threshstat="none", plot=TRUE, ...) 
{
  if (is.null(QC_good)) {
    QC_good <- as.numeric(!is.na(tsx))
  }
  else {
    QC_good <- as.numeric(QC_good)
  }
  x <- as.numeric(tsx)
  x[QC_good == 0] <- NA
  if (all(is.na(x)))
  {
    warning("Input is all NA")
    return(x)
  }
  if (plot)
    plot(x, type="o", ...)
  if (is.null(dates)) {
    dates <- index(tsx)
  }
  dates <- as.numeric(dates)
  loe <- try(loess(formula = x ~ dates, na.action = "na.omit", span=span, family=family))
  if (class(loe) == "try-error")
    return(x)
  loe_pred <- predict(loe, dates)
  if (plot)
    lines(loe_pred, col="green")
  distance <- (loe_pred - x)
  predmae = mean(abs(distance), na.rm=TRUE)
  predrmse = sqrt(mean(distance^2, na.rm=TRUE))
  xsd = sd(x, na.rm=TRUE)
  xmad = mad(x, na.rm=TRUE)
  if (plot)
    title(sub=paste("MAE:", round(predmae), "RMSE:", round(predrmse), "sd:", round(xsd), "mad:", round(xmad)))
  threshstat = switch(threshstat, none=1, sd=xsd, mad=xmad, mae=predmae, rmse=predrmse)
  threshold = threshold * threshstat
  if (!is.null(threshold)) {
    QC_good[distance < threshold[1] & !is.na(distance)] <- 2
    QC_good[distance > threshold[2] & !is.na(distance)] <- 2
  }
  if (class(tsx) == "zoo") {
    tsx <- zoo(cbind(x = as.numeric(tsx), QC_good, filled = loe_pred), 
               index(tsx))
    return(tsx)
  }
  else {
    x_omit <- x
    x_omit[QC_good != 1] <- NA
    if (plot)
      points(x_omit, type="o", col="red")
    res <- switch(res_type, all = data.frame(x = as.numeric(tsx), 
                                             QC_good = QC_good, filled = loe_pred, distance = round(distance)), 
                  filled = loe_pred, omit = x_omit, QC = QC_good, distance = distance, 
                  sd_distance = (distance/sd(x, na.rm = T)))
    return(res)
  }
}
