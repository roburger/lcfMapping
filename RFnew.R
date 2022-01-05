# MSc Thesis
# 21/12/2021
# Random Forest tryout pt 2

# Access libraries
library(sf)
library(ranger)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Link to data
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

# Read in training data (trainY)
filename = "../data/raw/training_data_2015_100m_20190402_V4_New.csv" #use this csv!!!
samplePoints = read.csv2(filename, header = T, sep = ",", dec = ".") # creates num columns
samplePoints$ï..rowid = NULL #remove duplicate ID column

# Read in features: NDVI harmonics (trainX)
harmonics = st_read(paste0(linkData, "processed/IIASAtrainingHarmonics.gpkg"), "NDVI")
st_geometry(harmonics)=NULL

# Get locationIDs to match datasets
locationID = read.csv("../data/processed/IIASAtrainingLocationID.csv")
harmonics = cbind(locationID,harmonics)
rm(locationID)
all(samplePoints$location_id %in% harmonics$location_id)

# now try to match samplePoints with harmonics based on locationID
# cbind will not work here, because they are not ordered the same

# Use selection
n=100000
chosen = sample(unique(samplePoints$location_id), n)

ransubsetY = subset(samplePoints, location_id %in% chosen)[,c("location_id","tree")]
ransubsetX = subset(harmonics, location_id %in% chosen)
all(ransubsetY$location_id %in% ransubsetX$location_id)

# split into training and test set
id80 = sample(unique(chosen),0.8*n)
id20 = chosen[which(!chosen %in% id80)]
sum(id80 %in% id20)
sum(id20 %in% id80)

subtrainY = subset(ransubsetY, location_id %in% id80)
subtrainX = subset(ransubsetX, location_id %in% id80)
all(subtrainY$location_id %in% subtrainX$location_id)
all(subtrainX$location_id %in% subtrainY$location_id)
dataTrain = merge(subtrainY,subtrainX)

#subtrainY = ransubsetY[1:(n*0.8),]
#subtrainX = ransubsetX[1:(n*0.8),]
#dataTrain = cbind(subtrainY,subtrainX) # moet merge worden

# testx and testy do not have same locationIDs
testX = subset(ransubsetX, location_id %in% id20)
testY = subset(ransubsetY, location_id %in% id20)
all(testX$location_id %in% testY$location_id)
all(testY$location_id %in% testX$location_id)

# Replace NA values with -9999
dataTrain[is.na(dataTrain)] = -9999
testX[is.na(testX)] = -9999

# Train RF model
print("Training the model...")
rfmodel = ranger(dataTrain$tree ~ ., data=dataTrain[!names(dataTrain) %in% c("tree","location_id","x","y")])

output = predict(rfmodel, testX[!names(testX) %in% c("location_id","x","y")])

# compare predictions with testY
compareDF = data.frame(location_id=testY$location_id, 
                       actual=testY$tree, pred=output$predictions)

sqrt(mean((compareDF$pred-compareDF$actual)^2)) #RMSE: 35.3 -> 34.1 (n=10.000)
mean(abs(compareDF$pred-compareDF$actual)) #MAE: 31.3 -> 27.2 (n=10.000)
# n=100.000 RMSE=47.2 MAE=36.1



# # 
# Try-out below
colnames(samplePoints)

all(samplePoints$x %in% harmonics$x)
all(samplePoints$location_id %in% ndvi$location_id)


which(!samplePoints$x %in% harmonics$x)
samplePoints[407,]$x %in% harmonics$x
which(harmonics$x == 20.1250496)


all(samplePoints$location_id %in% harmonics$location_id)

# try out merge instead of cbind 
# first one seems to work well
combiTest = merge(samplePoints, harmonics)
combiTest2 = merge(samplePoints, harmonics, by=c("location_id","x","y")) #looks like the same as above
combiTest3 = merge(samplePoints, harmonics, by="location_id", all = TRUE, no.dups = TRUE)
combiTest4 = merge(harmonics, samplePoints)


# do some first RF testing without merge result
n=100000
chosen = sample(unique(samplePoints$location_id), n)

chosen= samplePoints$location_id[1:100000]
# try to select first 10000
# try below to exclude 0's and 100's
chosen = samplePoints[samplePoints$tree>0&samplePoints$tree<100,"location_id"]
n = length(chosen)

ransubsetY = subset(samplePoints, location_id %in% chosen)[,c("location_id","tree")]
ransubsetX = subset(harmonics, location_id %in% chosen)
all(ransubsetY$location_id %in% ransubsetX$location_id)

# split into training and test set
id80 = sample(unique(chosen),as.integer(0.8*n))
id20 = chosen[which(!chosen %in% id80)]
sum(id80 %in% id20)
sum(id20 %in% id80)

subtrainY = subset(ransubsetY, location_id %in% id80)
subtrainX = subset(ransubsetX, location_id %in% id80)
all(subtrainY$location_id %in% subtrainX$location_id)
all(subtrainX$location_id %in% subtrainY$location_id)
dataTrain = merge(subtrainY,subtrainX)

#subtrainY = ransubsetY[1:(n*0.8),]
#subtrainX = ransubsetX[1:(n*0.8),]
#dataTrain = cbind(subtrainY,subtrainX) # moet merge worden

# testx and testy do not have same locationIDs
testX = subset(ransubsetX, location_id %in% id20)
testY = subset(ransubsetY, location_id %in% id20)
all(testX$location_id %in% testY$location_id)
all(testY$location_id %in% testX$location_id)

#testX = ransubsetX[(n*0.8+1):n,]
#testY = ransubsetY[(n*0.8+1):n,]

# Replace NA values with -9999
sum(is.na(dataTrain))
sum(is.na(testX))
testX$trend[4358] = 0 #only NA 

dataTrain[is.na(dataTrain)] = -9999
testX[is.na(testX)] = -9999

# Train RF model
print("Training the model...")
rfmodel = ranger(dataTrain$tree ~ ., importance = "impurity",
                 data=dataTrain[!names(dataTrain) %in% c("tree","location_id","x","y")])
output = predict(rfmodel, testX[!names(testX) %in% c("location_id","x","y")])
# testx and testy not matching
all(testX$location_id %in% testY$location_id)



# compare predictions with testY
compareDF = data.frame(location_id=testY$location_id, 
                       actual=testY$tree, pred=output$predictions)

sqrt(mean((compareDF$pred-compareDF$actual)^2)) #RMSE: 35.3 -> 34.1 (n=10.000)
mean(abs(compareDF$pred-compareDF$actual)) #MAE: 31.3 -> 27.2 (n=10.000)

# n=100.000 RMSE=47.2 MAE=36.1

# check if chosen are correct and match x and y
head(chosen, 5)
which(samplePoints$location_id %in% head(chosen, 5))
samplePoints[which(samplePoints$location_id == head(chosen, 5))[1], "tree"]


# try variable importance
library(varImp)
varImp(rfmodel)
rfmodel$variable.importance
