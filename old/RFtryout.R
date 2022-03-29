# MSc Thesis
# 21/12/2021
# Random Forest tryout

# Access libraries
library(sf)
library(ranger)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

# Retrieve band layers
#linkData <- "C:/Users/robur/Documents/Thesis/code/data/raw/IIASATraining2015_Landsat8_TS.gpkg"
linkLandsatIIASA2015 <- paste0(linkData, "raw/IIASATraining2015_Landsat8_TS.gpkg")
nameBands <- st_layers(linkLandsatIIASA2015)

# Read in data per band
b1Landsat <- st_read(linkLandsatIIASA2015, nameBands$name[1])
st_geometry(b1Landsat)=NULL

# Open data
trainY = read.csv("../data/raw/training_data_2015_100m_20190402_V4.csv", sep=";")
ndvi = st_read("../data/processed/IIASAtrainingVIs.gpkg", "NDVI")
st_geometry(ndvi) = NULL
ndvi = ndvi[,NewColDates]
locationID = read.csv("../data/processed/IIASAtrainingLocationID.csv")
ndvi = cbind(locationID, ndvi)

# Check if trainX corresponds with trainY
all(trainY$location_id %in% b1Landsat$location_id)
all(trainY$location_id %in% ndvi$location_id)

# Convert tree class
trainY$tree = as.numeric(as.character(trainY$tree))


# try to make a subset
subsetY = trainY[trainY$tree>30&trainY$tree<50,c("location_id", "tree")]
length(trainY[trainY$tree>30&trainY$tree<50,"tree"])
length(subsetY$location_id)

ndvi[ndvi$location_id %in% subsetY$location_id,"location_id"]
length(ndvi[ndvi$location_id %in% subsetY$location_id,"location_id"])
subsetX = ndvi[ndvi$location_id %in% subsetY$location_id,]

# Random select values from DFs
chosen <- sample(unique(trainY$location_id), 1000)
chosen
ransubsetY = subset(trainY, location_id %in% chosen)$tree
ransubsetX = subset(ndvi, location_id %in% chosen)

# nextup: split subsetX and subsetY into training and test set for RF
subtrainY = ransubsetY[1:800]
subtrainX = ransubsetX[1:800,]
dataTrain = cbind(subtrainY,subtrainX)

# Ranger exploration
?ranger
form = paste0(trainY$tree, "~", ndvi[1:10])
rfmodel = ranger(subtrainY ~ ., data=temp[!names(temp) %in% "location_id"])
all(is.na(dataTrain))

DropRows = apply(dataTrain, 1, function(x){any(!is.finite(x))})
apply(temp, 2, function(x){sum(is.na(x))}) / nrow(temp) * 100

temp = dataTrain
temp[is.na(temp)] = -9999
sum(is.na(temp))

testX = ransubsetX[801:1000,]
testY = ransubsetY[801:1000]
testX[is.na(testX)] = -9999

output = predict(rfmodel, testX)
output$predictions
testY


RFmodel <- function(ndvi, trainY, n=1000){
  
  chosen = sample(unique(trainY$location_id), n)
  ransubsetY = subset(trainY, location_id %in% chosen)$tree
  ransubsetX = subset(ndvi, location_id %in% chosen)
  
  # split into training and test set
  subtrainY = ransubsetY[1:(n*0.8)]
  subtrainX = ransubsetX[1:(n*0.8),]
  dataTrain = cbind(subtrainY,subtrainX)
  
  testX = ransubsetX[(n*0.8+1):n,]
  testY = ransubsetY[(n*0.8+1):n]
  
  # Replace NA values with -9999
  dataTrain[is.na(dataTrain)] = -9999
  testX[is.na(testX)] = -9999
  
  # Train RF model
  print("Training the model...")
  rfmodel = ranger(subtrainY ~ ., data=dataTrain[!names(dataTrain) %in% "location_id"])
  
  # Predict outcomes
  output = predict(rfmodel, testX)
  output$predictions
  testY
  
  return(output)
}

output = RFmodel(ndvi, trainY, n=10000)
output$predictions
testY



# Sample but no 0's and 100's
trainY[trainY$tree]$location_id
trainY$tree>0 & trainY$tree<100
length(trainY[trainY$tree>0 & trainY$tree<100,]$tree)
length(trainY$tree)
# around 31,7% is not 0 or 100

temp = trainY[trainY$tree>0 & trainY$tree<100,]
n = 40000
chosen = sample(unique(temp$location_id), n)

ransubsetY = subset(temp, location_id %in% chosen)$tree
ransubsetX = subset(ndvi, location_id %in% chosen)

# split into training and test set
subtrainY = ransubsetY[1:(n*0.8)]
subtrainX = ransubsetX[1:(n*0.8),]
dataTrain = cbind(subtrainY,subtrainX)

testX = ransubsetX[(n*0.8+1):n,]
testY = ransubsetY[(n*0.8+1):n]

# Replace NA values with -9999
dataTrain[is.na(dataTrain)] = -9999
testX[is.na(testX)] = -9999

# Train RF model
print("Training the model...")
rfmodel = ranger(subtrainY ~ ., data=dataTrain[!names(dataTrain) %in% c("location_id","x","y")])

# Predict outcomes
output = predict(rfmodel, testX)
#output$predictions
#testY

# compare predictions with testY
compareDF = data.frame(actual=testY, pred=output$predictions)

sqrt(mean((compareDF$pred-compareDF$actual)^2)) #RMSE: 35.3 (n=10.000)
mean(abs(compareDF$pred-compareDF$actual)) #MAE: 31.3 (n=10.000)

# # # # 
# Include more features
Harm = st_read(paste0(linkData, "processed/IIASAtrainingHarmonics.gpkg"), "NDVI")
st_geometry(Harm)=NULL
harm = cbind(locationID, Harm)
sum(is.na(harm[,4]))

ransubsetX = subset(harm, location_id %in% chosen)

#probably the coords are not assigned to the correct locationIDs
# fixed the above now in RFnew script