# MSc Thesis
# 27/01/2021
# Split time series into yearly forecasts

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Access libraries
library(sf)
#library(devtools)
library(probaV)
library(pbapply)
library(ranger)
source("utils/extractDates.R")
source("utils/dataManagement.R")
source("utils/loadData.R")
source("RFfunction.R")

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

## Apply on Training set first ##

# Read in filtered bands
b1 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b1")
b2 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b2")
b3 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b3")
b4 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b4")
b5 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b5")
b6 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b6")
b7 = st_read(paste0(linkData, "processed/IIASAtrainingFiltered.gpkg"), "b7")

st_geometry(b1)=NULL
st_geometry(b2)=NULL
st_geometry(b3)=NULL
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

b12015 = b1[,colnames(b1)[grepl("2014|2015|2016", colnames(b1))]]
b12016 = b1[,colnames(b1)[grepl("2015|2016|2017", colnames(b1))]]
b12017 = b1[,colnames(b1)[grepl("2016|2017|2018", colnames(b1))]]
b12018 = b1[,colnames(b1)[grepl("2017|2018|2019", colnames(b1))]]

# extract bands for each year -> TODO: make function
# 2015
# 68 observations / dates
b12015 = b1[,colnames(b1)[grepl("2014|2015|2016", colnames(b1))]]
b22015 = b2[,colnames(b2)[grepl("2014|2015|2016", colnames(b2))]]
b32015 = b3[,colnames(b3)[grepl("2014|2015|2016", colnames(b3))]]
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]]
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]
b62015 = b6[,colnames(b6)[grepl("2014|2015|2016", colnames(b6))]]
b72015 = b7[,colnames(b7)[grepl("2014|2015|2016", colnames(b7))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b42015, as.numeric))
b5Temp = as.data.frame(sapply(b52015, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates= extractDates()
dates = dates[grepl("2014|2015|2016",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))
HarmMetrics = cbind(coordsData[, c("x", "y")], HarmMetrics)

# change colnames
names(HarmMetrics)[3:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi)2015 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2015/IIASAtrainingHarmonics.gpkg", "NDVI")
harmonics2015 = st_read("../data/processed/2015/IIASAtrainingHarmonics.gpkg", "NDVI")
st_geometry(harmonics2015)=NULL

# load training data with function
dataTrain = loadTrainingData("2015")

# perform RF
predictions = runRandomForest(dataTrain)
statistics = getStats(predictions)

# save RF predictions
write.csv(testsScale, "../data/output/predictions-2015-ndvi.csv", row.names = F)
predCSV = read.csv("../data/output/predictions-2015-ndvi.csv")


# 2016 #
# 69 observations / dates
b42016 = b4[,colnames(b4)[grepl("2015|2016|2017", colnames(b4))]]
b52016 = b5[,colnames(b5)[grepl("2015|2016|2017", colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b42016, as.numeric))
b5Temp = as.data.frame(sapply(b52016, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates= extractDates()
dates = dates[grepl("2015|2016|2017",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/IIASAtrainingCoords.csv"))
HarmMetrics = cbind(coordsData[, c("x", "y")], HarmMetrics)

# change colnames
names(HarmMetrics)[3:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2016 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2016/IIASAtrainingHarmonics.gpkg", "NDVI")
harmonics2016 = st_read("../data/processed/2016/IIASAtrainingHarmonics.gpkg", "NDVI")
st_geometry(harmonics2016)=NULL

# load training data with function
dataTrain = loadTrainingData("2016")

# perform RF
predictions = runRandomForest(dataTrain)
statistics = getStats(predictions)

# save RF predictions
write.csv(predictions, "../data/output/predictions-2016-ndvi.csv", row.names = F)
predCSV = read.csv("../data/output/predictions-2016-ndvi.csv")


### EDIT: I SPLIT TRAINING, I SHOULD SPLIT VALIDATION (TESTX)
# So change all of the above...
# Also change the loadData part where loadTraining(year=2015) -> loadTraining() & loadVali(year=2015)

b4 = st_read(paste0(linkData, "processed/WURvalidationFiltered.gpkg"), "b4")
b5 = st_read(paste0(linkData, "processed/WURvalidationFiltered.gpkg"), "b5")
st_geometry(b4)=NULL
st_geometry(b5)=NULL

# 2015 #
# 68 observations / dates
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]]
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b42015, as.numeric))
b5Temp = as.data.frame(sapply(b52015, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates= extractDates()
dates = dates[grepl("2014|2015|2016",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/WURvalidationIDcoords.csv"))
HarmMetrics = cbind(coordsData, HarmMetrics)

# change colnames
names(HarmMetrics)[2:(length(HarmMetrics))]= c("x", "y", "min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2015 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2015/WURvalidationHarmonics.gpkg", "NDVI")
harmonics2015 = st_read("../data/processed/2015/WURvalidationHarmonics.gpkg", "NDVI")
st_geometry(harmonics2015)=NULL

# load training data with function
dataTrain = loadTrainingData()
dataVali = loadValidationData("2015")

# perform RF
predictions = runRandomForest(train=dataTrain, vali=dataVali)
statistics = getStats(predictions, dataVali)

# save RF predictions
write.csv(predictions, "../data/output/predictions-2015-ndvi.csv", row.names = F)
pred2015 = read.csv("../data/output/predictions-2015-ndvi.csv")
vali2015 = loadValidationData("2015")
pred2015 = cbind(vali2015$sample_id, pred2015)


# 2016 #
# 69 observations / dates
string2016 = "2015|2016|2017"
b4Temp = b4[,colnames(b4)[grepl(string2016, colnames(b4))]]
b5Temp = b5[,colnames(b5)[grepl(string2016, colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b4Temp, as.numeric))
b5Temp = as.data.frame(sapply(b5Temp, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates = extractDates()
dates = dates[grepl(string2016,dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/WURvalidationIDcoords.csv"))
HarmMetrics = cbind(coordsData, HarmMetrics)

# change colnames
names(HarmMetrics)[2:(length(HarmMetrics))]= c("x", "y", "min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2016 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2016/WURvalidationHarmonics.gpkg", "NDVI")
harmonics2016 = st_read("../data/processed/2016/WURvalidationHarmonics.gpkg", "NDVI")
st_geometry(harmonics2016)=NULL

# load training data with function
dataTrain = loadTrainingData()
dataVali = loadValidationData("2016")

# perform RF
predictions = runRandomForest(train=dataTrain, vali=dataVali)
statistics = getStats(predictions, dataVali)

# save RF predictions
write.csv(predictions, "../data/output/predictions-2016-ndvi.csv", row.names = F)
predCSV = read.csv("../data/output/predictions-2016-ndvi.csv")

# 2017 #
# 69 observations / dates
string2017 = "2016|2017|2018"
b4Temp = b4[,colnames(b4)[grepl(string2017, colnames(b4))]]
b5Temp = b5[,colnames(b5)[grepl(string2017, colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b4Temp, as.numeric))
b5Temp = as.data.frame(sapply(b5Temp, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates = extractDates()
dates = dates[grepl(string2017,dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/WURvalidationIDcoords.csv"))
HarmMetrics = cbind(coordsData, HarmMetrics)

# change colnames
names(HarmMetrics)[2:(length(HarmMetrics))]= c("x", "y", "min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2017 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2017/WURvalidationHarmonics.gpkg", "NDVI")
harmonics2017 = st_read("../data/processed/2017/WURvalidationHarmonics.gpkg", "NDVI")
st_geometry(harmonics2017)=NULL

# load training data with function
dataTrain = loadTrainingData()
dataVali = loadValidationData("2017")

# perform RF
predictions = runRandomForest(train=dataTrain, vali=dataVali)
statistics = getStats(predictions, dataVali)

# save RF predictions
write.csv(predictions, "../data/output/predictions-2017-ndvi.csv", row.names = F)
pred2017 = read.csv("../data/output/predictions-2017-ndvi.csv")

# 2018 #
# 68 observations / dates
string2018 = "2017|2018|2019"
b4Temp = b4[,colnames(b4)[grepl(string2018, colnames(b4))]]
b5Temp = b5[,colnames(b5)[grepl(string2018, colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b4Temp, as.numeric))
b5Temp = as.data.frame(sapply(b5Temp, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates = extractDates()
dates = dates[grepl(string2018,dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))

# adjust temporal harmonics
coordsData = read.csv(paste0(linkData, "processed/WURvalidationIDcoords.csv"))
HarmMetrics = cbind(coordsData, HarmMetrics)

# change colnames
names(HarmMetrics)[2:(length(HarmMetrics))]= c("x", "y", "min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2018 features as GPKG
temp = DFtoSF(HarmMetrics)
st_write(temp, "../data/processed/2018/WURvalidationHarmonics.gpkg", "NDVI")
harmonics2018 = st_read("../data/processed/2018/WURvalidationHarmonics.gpkg", "NDVI")
st_geometry(harmonics2018)=NULL

# load training data with function
dataTrain = loadTrainingData()
dataVali = loadValidationData("2018")

# perform RF
predictions = runRandomForest(train=dataTrain, vali=dataVali)
statistics = getStats(predictions, dataVali)

# save RF predictions
write.csv(predictions, "../data/output/predictions-2018-ndvi.csv", row.names = F)
pred2018 = read.csv("../data/output/predictions-2018-ndvi.csv")


## SAME SEED FOR RF? PROBABLY YEAH...
# SAME RF MODEL FOR EACH YEAR.?
# SO, RF-MODEL FOR 'TREE', PREDICT FOR 2015, 2016, 2017 AND 2018.
dataTrain = loadTrainingData()
dataVali2015 = loadValidationData("2015")
dataVali2016 = loadValidationData("2016")
dataVali2017 = loadValidationData("2017")
dataVali2018 = loadValidationData("2018")


classes = loadClassNames()
features = loadNDVIcovarsNames()
subformula = paste0("~", paste(features, collapse = "+"))

# Create empty df for predictions
predictions2018 = data.frame(matrix(ncol=length(classes), nrow=nrow(dataVali2015)))
colnames(predictions2018)=classes

formula = paste0(classes[1], subformula)
rfmodel = ranger(formula, dataTrain)
output = predict(rfmodel, dataVali[,features])$predictions
predictions[,"tree"]=output

Before - nrow(dataVali2015)
# todo: get similar rows for all years
sum(dataVali2015$sample_id %in% dataVali2016$sample_id)
sum(dataVali2015$sample_id %in% dataVali2017$sample_id)
sum(dataVali2015$sample_id %in% dataVali2018$sample_id)

temp = subset(dataVali2015, sample_id %in% dataVali2016$sample_id)
length(temp$sample_id)

temp = subset(temp, sample_id %in% dataVali2017$sample_id)
length(temp$sample_id)

temp = subset(temp, sample_id %in% dataVali2018$sample_id)
length(temp$sample_id)

all(temp$sample_id %in% dataVali2015$sample_id)
all(temp$sample_id %in% dataVali2016$sample_id)
all(temp$sample_id %in% dataVali2017$sample_id)
all(temp$sample_id %in% dataVali2018$sample_id)

dataVali2015 = subset(dataVali2015, sample_id %in% temp$sample_id)
dataVali2016 = subset(dataVali2016, sample_id %in% temp$sample_id)
dataVali2017 = subset(dataVali2017, sample_id %in% temp$sample_id)
dataVali2018 = subset(dataVali2018, sample_id %in% temp$sample_id)

output2015 = predict(rfmodel, dataVali2015[,features])$predictions
predictions2015[,"tree"]=output2015

output2016 = predict(rfmodel, dataVali2016[,features])$predictions
predictions2016[,"tree"]=output2016

output2017 = predict(rfmodel, dataVali2017[,features])$predictions
predictions2017[,"tree"]=output2017

output2018 = predict(rfmodel, dataVali2018[,features])$predictions
predictions2018[,"tree"]=output2018
 
# work on RFfunction... 
# todo: scale all fractions for each year
# todo: years = list(datavali2015, datavali2016, ...)
# todo (not vital now): add seed in ranger

years = list(dataVali2015, dataVali2016, dataVali2017, dataVali2018)
listDFs = runRandomForest(train=dataTrain, years=years)
pred2015 = listDFs[[1]]
pred2016 = listDFs[[2]]
pred2017 = listDFs[[3]]
pred2018 = listDFs[[4]]
write.csv(pred2018, "../data/output/predictions-2018-ndvi.csv", row.names = F)
pred2017 = read.csv("../data/output/predictions-2017-ndvi.csv")

sum(pred2015)
sum(pred2015$tree) / sum(pred2015) *100
sum(pred2016$tree) / sum(pred2016) *100
sum(pred2017$tree) / sum(pred2017) *100
sum(pred2018$tree) / sum(pred2018) *100

# TODO: get stats
stats2015 = getStats(pred2015, dataVali2015)
stats2016 = getStats(pred2016, dataVali2015)
stats2017 = getStats(pred2017, dataVali2015)
stats2018 = getStats(pred2018, dataVali2015)

rmse = data.frame(matrix(ncol=length(classes), nrow=4))
colnames(rmse)=classes
rownames(rmse)=c("2015","2016","2017","2018")
rmse["2015",]=medstats2015["RMSE",]
rmse["2016",]=medstats2016["RMSE",]
rmse["2017",]=medstats2017["RMSE",]
rmse["2018",]=medstats2018["RMSE",]

mae = data.frame(matrix(ncol=length(classes), nrow=4))
colnames(mae)=classes
rownames(mae)=c("2015","2016","2017","2018")
mae["2015",]=medstats2015["MAE",]
mae["2016",]=medstats2016["MAE",]
mae["2017",]=medstats2017["MAE",]
mae["2018",]=medstats2018["MAE",]

# TODO: get Truth data in CSV
truth2015 = dataVali2015[,classes]
truth2016 = dataVali2016[,classes]
truth2017 = dataVali2017[,classes]
truth2018 = dataVali2018[,classes]
write.csv(truth2018,"../data/processed/2018/truth-2018.csv", row.names = F)


# TODO: next-up. 
# look at variable importance?
# fancy-up scripts

# Median voting
years = list("2015"=dataVali2015, "2016"=dataVali2016,"2017"=dataVali2017, "2018"=dataVali2018)
listDFs = runRandomForest(train=dataTrain, years=years, PredictType="quantiles")
medianPred2015 = listDFs[[1]]
medianPred2016 = listDFs[[2]]
medianPred2017 = listDFs[[3]]
medianPred2018 = listDFs[[4]]

# TODO: get stats
medianStats2015 = getStats(medianPred2015, dataVali2015)
medianStats2016 = getStats(medianPred2016, dataVali2015)
medianStats2017 = getStats(medianPred2017, dataVali2015)
medianStats2018 = getStats(medianPred2018, dataVali2015)

write.csv(medianPred2015, "../data/output/predictions-2015-ndvi-median.csv", row.names = F)
write.csv(medianPred2016, "../data/output/predictions-2016-ndvi-median.csv", row.names = F)
write.csv(medianPred2017, "../data/output/predictions-2017-ndvi-median.csv", row.names = F)
write.csv(medianPred2018, "../data/output/predictions-2018-ndvi-median.csv", row.names = F)
pred2015 = read.csv("../data/output/wurChange/predictions-2015-ndvi-median.csv")
pred2016 = read.csv("../data/output/wurChange/predictions-2016-ndvi-median.csv")
pred2017 = read.csv("../data/output/wurChange/predictions-2017-ndvi-median.csv")
pred2018 = read.csv("../data/output/wurChange/predictions-2018-ndvi-median.csv")
truth = read.csv("../data/processed/2015/truth-2015.csv")
rmse2015 = sqrt(mean(unlist(pred2015 - truth)^2))
rmse2016 = sqrt(mean(unlist(pred2016 - truth)^2))
rmse2017 = sqrt(mean(unlist(pred2017 - truth)^2))
rmse2018 = sqrt(mean(unlist(pred2018 - truth)^2))
mae2015 = mean(abs(unlist(pred2015  - truth)))
mae2016 = mean(abs(unlist(pred2016  - truth)))
mae2017 = mean(abs(unlist(pred2017  - truth)))
mae2018 = mean(abs(unlist(pred2018  - truth)))

# median voting seems to return NaNs...
# not sure why, did not happen in normal RF (with mean voting)
sum(!(is.finite(rowSums(medianPred2015))))
# check line 401 in classify-rf

medianPred2015[is.na(medianPred2015)] = 100/length(classes)
medianPred2016[is.na(medianPred2016)] = 100/length(classes)
medianPred2017[is.na(medianPred2017)] = 100/length(classes)
medianPred2018[is.na(medianPred2018)] = 100/length(classes)

# maybe leave out the NaNs.. bc stats do not look good
# however, rmse worse, mae better...
medianPred2015 = read.csv("../data/output/predictions-2015-ndvi-median.csv")
medianPred2016 = read.csv("../data/output/predictions-2016-ndvi-median.csv")
medianPred2017 = read.csv("../data/output/predictions-2017-ndvi-median.csv")
medianPred2018 = read.csv("../data/output/predictions-2018-ndvi-median.csv")



apply(dataTrain, 2, function(x){sum(is.na(x))})
sum(is.na(dataTrain))
apply(dataVali2015[,features], 2, function(x){sum(is.na(x))})
