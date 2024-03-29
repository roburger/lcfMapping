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
samplePoints$�..rowid = NULL #remove duplicate ID column

# Read in features: NDVI harmonics (trainX)
harmonics = st_read(paste0(linkData, "processed/IIASAtrainingHarmonics.gpkg"), "NDVI")
st_geometry(harmonics)=NULL

# Get locationIDs to match datasets
locationID = read.csv("../data/processed/IIASAtrainingLocationID.csv")
harmonics = cbind(locationID,harmonics)
rm(locationID)
all(samplePoints$location_id %in% harmonics$location_id)

training = cbind(harmonics,iqr)
training = cbind(training, median)
training = harmonics
# now try to match samplePoints with harmonics based on locationID
# cbind will not work here, because they are not ordered the same

# Use selection
n=150000
chosen = sample(unique(samplePoints$location_id), n)

ransubsetY = subset(samplePoints, location_id %in% chosen)[,c("location_id","tree")]
#ransubsetX = subset(harmonics, location_id %in% chosen)
ransubsetX = subset(training, location_id %in% chosen)
all(ransubsetY$location_id %in% ransubsetX$location_id)
all(ransubsetX$location_id %in% ransubsetY$location_id)

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
sum(is.na(dataTrain))
sum(is.na(testX))
dataTrain[is.na(dataTrain)] = -9999
testX[is.na(testX)] = -9999

# Train RF model
print("Training the model...")
rfmodel = ranger(dataTrain$tree ~ ., importance = "impurity",
                 data=dataTrain[!names(dataTrain) %in% c("tree","location_id","x","y")])

output = predict(rfmodel, testX[!names(testX) %in% c("location_id","x","y")])

# output$predictions and testY not have same locationID, wrong compareDF?
temp = data.frame(location_id=testX$location_id, pred=output$predictions)
all(testY$location_id %in% temp$location_id)
compareDF = merge(testY, temp)
rm(temp)

# Accuracy Assessment RMSE MAE
sqrt(mean((compareDF$pred-compareDF$tree)^2)) #RMSE: 35.3 -> 34.1 (n=10.000)
mean(abs(compareDF$pred-compareDF$tree)) #MAE: 31.3 -> 27.2 (n=10.000)
# n=100.000 RMSE=47.2 MAE=36.1
# n=length(0<tree<100)=47679 RMSE=37.9 MAE=31.3
# n=50.000 RMSE=25.5 MAE=17.7

# n=12.000 RMSE=24.0 MAE=15.7 (with IQR and median NDVI)




# # 
# Try-out below

# Check NA's per columns
apply(harmonics, 2, function(x){sum(is.na(x))}) / nrow(harmonics) * 100

# TidyData check
DropRows = apply(harmonics, 1, function(x){any(!is.finite(x))})
sum(DropRows)
nrow(temp[!DropRows,])
nrow(temp[DropRows,])

# Try to merge samplePoints and Harmonics
temp = merge(samplePoints,harmonics)
apply(temp, 2, function(x){sum(is.na(x))}) / nrow(temp) * 100
DropRows = apply(temp[,colnames(harmonics)], 1, function(x){any(!is.finite(x))})
temp = temp[!DropRows,]
all(apply(temp, 2, function(x){sum(is.na(x))}) / nrow(temp) * 100 == 0)

temp = temp[temp$dominant_lc != "not_sure",]


#temp = merge(samplePoints,harmonics, by="location_id")

subset(samplePoints, !(samplePoints$y %in% temp$y))$y
subset(samplePoints, !(samplePoints$x %in% temp$x))$x
#subset(samplePoints, !(samplePoints$location_id %in% temp$location_id))$location_id

subset(harmonics, !(harmonics$y %in% temp$y))$y
subset(harmonics, !(harmonics$x %in% temp$x))$x
#subset(harmonics, !(harmonics$location_id %in% temp$location_id))$location_id

subset(harmonics, !(harmonics$y %in% samplePoints$y))$y
subset(harmonics, !(harmonics$x %in% samplePoints$x))$x
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))


##
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
#sample 10.000 from above (0<tree<100)
chosen = sample(unique(samplePoints[samplePoints$tree>0&samplePoints$tree<100,"location_id"]),10000)
n=10000

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

# try histograms
hist(compareDF$tree, breaks = 100, xlab = "actual", main = "Histogram of actual")
val = hist(compareDF$actual, breaks = 100)
val$counts
hist(compareDF$pred, breaks = 100, xlab = "predicted", main = "Histogram of predictions")

plot(density(compareDF$tree),col='red', main="Kernel density plot", xlab="Red: actual, Blue: predicted")
lines(density(compareDF$pred),col='blue')

plot(compareDF$pred,compareDF$tree, xlab = "actual", ylab = "predicted")
lines(c(1:100),c(1:100), col="red", lwd = 2)

plot(tempCompare$pred, tempCompare$tree, xlab="Predicted", ylab="Actual")
