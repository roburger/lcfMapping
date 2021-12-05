# MSc Thesis
# 29/11/2021
# Read in data

# Set working directory
setwd("~/Thesis/code")

# Import packages
library(sf)
library(dplyr)
library(pbapply)

# Retrieve band layers
linkData <- "./data/raw/IIASATraining2015_Landsat8_TS.gpkg"
nameBands <- st_layers(linkData)

# Read in data per band
b1Landsat <- st_read(linkData, nameBands$name[1])
b2Landsat <- st_read(linkData, nameBands$name[2])
b3Landsat <- st_read(linkData, nameBands$name[3])
b4Landsat <- st_read(linkData, nameBands$name[4])
b5Landsat <- st_read(linkData, nameBands$name[5])
b6Landsat <- st_read(linkData, nameBands$name[6])
b7Landsat <- st_read(linkData, nameBands$name[7])

# Explore data
names(b1Landsat)
plot(b1Landsat$geom, pch=20, cex=0.01)

# Test with first 5 data points
first5 <- TrainingData[0:5,]

first5 <- first5[1,4:194]
sum(is.na(first5))

plot(na.omit(first5))
df_transpose = t(first5)
plot(df_transpose[,])
df_transpose[,1]
names(df_transpose)
plot(first5[3], first5)

# Drop geometry
first5 <- first5 %>% st_set_geometry(NULL)


plot(na.omit(as.numeric(first5)))
names(first5)

b12015 <- select(b1Landsat, matches("2015"))
plot(as.numeric(b2Landsat[2,]))
b12015[1,]

# NDVI tryout
nirBand <- as.numeric(b5Landsat[1,4:194])
redBand <- as.numeric(b4Landsat[1,4:194])
ndvi <- (nirBand - redBand) / (nirBand + redBand)
plot(ndvi)


pbapply(as.matrix((nirBand - redBand) / (nirBand + redBand)), 2, median, cl=NULL)
pbapply(as.matrix(ndvi), na.rm=T, 2, median, cl=NULL)

median(unlist(ndvi), na.rm=T)

# Full NDVI time series
nirBand <- as.data.frame(b5Landsat[,4:194])
redBand <- as.data.frame(b4Landsat[,4:194])
ndvi <- (nirBand - redBand) / (nirBand + redBand)
pbapply(as.matrix(ndvi), 2, median, cl=NULL, na.rm=T)

plot(as.numeric(ndvi[1,]))

save(ndvi,file="./data/processed/ndvi.Rda")
load("./data/processed/ndvi.Rda")


# until 191, to exclude the geometry
ndviMedian = pbapply(as.matrix(as.data.frame(ndvi[,1:191])), 1, median, na.rm=T)
ndviIQR25 = pbapply(as.matrix(as.data.frame(ndvi[,1:191])), 1, quantile, probs=0.25, na.rm=T)
ndviIQR75 = pbapply(as.matrix(as.data.frame(ndvi[,1:191])), 1, quantile, probs=0.75, na.rm=T)

# NDMI
nirBand <- b5Landsat[,4:194]
swirBand <- b6Landsat[,4:194]
ndmi <- (nirBand - swirBand) / (nirBand + swirBand)
ndmiMedian = pbapply(as.matrix(as.data.frame(ndmi[,1:191])), 1, median, na.rm=T)
save(ndmi,file="./data/processed/ndmi.Rda")

# EVI
blueBand <- b2Landsat[,4:194]
st_geometry(nirBand) <- NULL
st_geometry(redBand) <- NULL # deze werkt niet ff error checken
st_geometry(blueBand) <- NULL

evi <- 2.5 * (nirBand - redBand) / (nirBand + 6*redBand - 7.5*blueBand + 1)
