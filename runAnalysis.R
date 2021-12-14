# MSc Thesis
# 13/12/2021
# Run all analysis

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Import packages
library(sf)
library(dplyr)
library(pbapply)

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

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

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
dates
ColDates = paste0("X", gsub("-", ".", dates), "_SR_B1")
NewColDates = paste0("X", gsub("-", ".", dates))


# Get XY coords
b1Test = b1Landsat
st_geometry(b1Test) <- NULL
DFcoords = b1Test[c("x","y")]
write.csv(DFcoords, paste0(linkData, "processed/IIASAtrainingCoords.csv"), row.names=F)

# Apply filter to remove outliers
source("utils/filterBands.R")
b1Filtered <- filterBands(b1Landsat, smoothLoessPlot, dates)
mean(is.na(b1Filtered))
mean(is.na(b1Values))

b1FilterTest = b1Filtered
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))
b1FilterTest = cbind(b1FilterTest, x=coordsData$x, y=coordsData$y)
coords = c("x","y")
tempSF = st_as_sf(b1FilterTest, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"
st_write(tempSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.gpkg")
write.csv(b1Filtered, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.csv")
# Create DF to SF function TODO


# Remove geometries
st_geometry(b1Landsat) = NULL
st_geometry(b2Landsat) = NULL
st_geometry(b3Landsat) = NULL
st_geometry(b4Landsat) = NULL
st_geometry(b5Landsat) = NULL

st_geometry(b6Landsat) = NULL
st_geometry(b7Landsat) = NULL


# change column names to make them match
colnames(b2Landsat)[4:194] = NewColDates
colnames(b3Landsat)[4:194] = NewColDates
colnames(b4Landsat)[4:194] = NewColDates
colnames(b5Landsat)[4:194] = NewColDates
colnames(b6Landsat)[4:194] = NewColDates
colnames(b7Landsat)[4:194] = NewColDates

b2Filtered <- filterBands(b2Landsat, smoothLoessPlot, dates)
b3Filtered <- filterBands(b3Landsat, smoothLoessPlot, dates)
b4Filtered <- filterBands(b4Landsat, smoothLoessPlot, dates)
b5Filtered <- filterBands(b5Landsat, smoothLoessPlot, dates)

mean(is.na(b2Filtered[,192:382]))
mean(is.na(b2Landsat[,NewColDates]))

mean(is.na(b3Filtered))
mean(is.na(b3Landsat[,NewColDates]))

mean(is.na(b4Filtered))
mean(is.na(b4Landsat[,NewColDates]))

mean(is.na(b5Filtered))
mean(is.na(b5Landsat[,NewColDates]))

# remove redundant columns, and change/restore colnames
b5Filtered = b5Filtered[,192:382]
colnames(b5Filtered) = NewColDates
colnames(b5Filtered)

# Write and store Filtered bands as SF
tempDF = b5Filtered
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))
tempDF = cbind(tempDF, x=coordsData$x, y=coordsData$y)
coords = c("x","y")
tempSF = st_as_sf(tempDF, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"
st_write(tempSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b5Filtered.gpkg")

# Remove new NAs for all bands....?
# Now done for the band itself...

# Load Filtered bands
b1Filtered = st_read(paste0(linkData, "processed/b1Filtered.gpkg"))
b2Filtered = st_read(paste0(linkData, "processed/b2Filtered.gpkg"))
b3Filtered = st_read(paste0(linkData, "processed/b3Filtered.gpkg"))
b4Filtered = st_read(paste0(linkData, "processed/b4Filtered.gpkg"))
b5Filtered = st_read(paste0(linkData, "processed/b5Filtered.gpkg"))

st_geometry(b1Filtered) = NULL
colnames(b1Filtered)[1:191] = NewColDates
st_geometry(b2Filtered) = NULL
st_geometry(b3Filtered) = NULL
st_geometry(b4Filtered) = NULL
st_geometry(b5Filtered) = NULL

b1Filtered = b1Filtered[,NewColDates]
b2Filtered = b2Filtered[,NewColDates]
b3Filtered = b3Filtered[,NewColDates]
b4Filtered = b4Filtered[,NewColDates]
b5Filtered = b5Filtered[,NewColDates]

b1Matrix = as.matrix(b1Filtered[,NewColDates])
b2Matrix = as.matrix(b2Filtered[,NewColDates])
b3Matrix = as.matrix(b3Filtered[,NewColDates])
b4Matrix = as.matrix(b4Filtered[,NewColDates])
b5Matrix = as.matrix(b5Filtered[,NewColDates])
b6Matrix = as.matrix(b6Landsat[,NewColDates])
b7Matrix = as.matrix(b7Landsat[,NewColDates])

mean(is.na(b2Matrix))
mean(is.na(b2Filtered))
mean(is.na(b2Landsat[,NewColDates]))

mean(is.na(b3Filtered))
mean(is.na(b3Landsat[,NewColDates]))

mean(is.na(b4Filtered))
mean(is.na(b4Landsat[,NewColDates]))

mean(is.na(b5Filtered))
mean(is.na(b5Landsat[,NewColDates]))

# Apply b2 filter to all bands
b1Matrix = as.matrix(b7Landsat)[,NewColDates]
b1Matrix[is.na(b2Matrix)] = NA

b7Filtered[,NewColDates] = b1Matrix
mean(is.na(b7Filtered))
mean(is.na(b2Filtered))

# TODO export new filtered bands + reorganize code + create functions
vars = paste0("b",1:7,"Filtered")

source("utils/dataManagement.R")

b1FilteredSF <- DFtoSF(b1Filtered)
b2FilteredSF <- DFtoSF(b2Filtered)
b3FilteredSF <- DFtoSF(b3Filtered)
b4FilteredSF <- DFtoSF(b4Filtered)
b5FilteredSF <- DFtoSF(b5Filtered)
b6FilteredSF <- DFtoSF(b6Filtered)
b7FilteredSF <- DFtoSF(b7Filtered)

st_write(b1FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.gpkg")
st_write(b2FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b2Filtered.gpkg")
st_write(b3FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b3Filtered.gpkg")
st_write(b4FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b4Filtered.gpkg")
st_write(b5FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b5Filtered.gpkg")
st_write(b6FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b6Filtered.gpkg")
st_write(b7FilteredSF, "C:/Users/robur/Documents/Thesis/code/data/processed/b7Filtered.gpkg")

# TODO save as one gpkd with mulitple layers?
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
