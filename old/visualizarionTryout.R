# MSc Thesis
# 23/12/2021
# Visualization tryout

# Access libraries
library(sf)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

coords = read.csv("../data/processed/IIASAtrainingCoords.csv")
plot(coords)

validation = read.csv("../data/raw/training_data_2015_100m_20190402_V4.csv", 
                      sep=";", dec=".")

coords[coords$x %in% subsetY$x,"x"]
length(coords[coords$x %in% subsetY$x,"location_id"])

source("utils/dataManagement.R")
DFtoSF(validation)
st_as_sf(validation, coords=c("x","y"), crs=4326)

validation = read.csv2("../data/raw/training_data_2015_100m_20190402_V4.csv", 
                      sep=";", dec=".")


validation = read.table("../data/raw/training_data_2015_100m_20190402_V4.csv", 
                      sep=";", dec=".", stringsAsFactors=F)
#in csv staan miljoen getallen in x

filename = "../data/raw/training_data_2015_100m_20190402_V4.csv"
samplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), 
                       stringsAsFactors = FALSE)
st_crs(samplePoints) = 4326

sum(is.na(samplePoints$x))
range(na.omit(samplePoints$y))

plot(samplePoints$geometry, pch=20, cex=0.01)
#samplePoints[is.na(samplePoints)] = -9999
sum(is.na(samplePoints))


# test new esxcel file
filename = "../data/raw/training_data_2015_100m_20190402_V4_New.csv" #use this csv!!!
samplePoints = read.csv2(filename, header = T, sep = ",", dec = ".") # creates num columns
samplePoints = st_read(filename, options=c("X_POSSIBLE_NAMES=x", "Y_POSSIBLE_NAMES=y"), 
                       stringsAsFactors = FALSE) # applies chr instead of num on columns

#as.numeric(sub(",", ".", samplePoints$tree, fixed = TRUE))
source("utils/dataManagement.R")
points = DFtoSF(samplePoints) # from read.csv to SF object
plot(points["tree"], pch=20, cex=0.01, key.pos=4, axes=T) # nice plot

# With st_read
st_crs(samplePoints) = 4326
samplePoints
plot(samplePoints["tree"], pch=20, cex=0.01, key.pos = 4)

