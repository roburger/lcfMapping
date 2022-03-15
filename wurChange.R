# MSc Thesis
# 09/02/2021
# Process WUR change data

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Access libraries
library(sf)
library(pbapply)
library(probaV)
library(ranger)

source("utils/filterBands.R")
source("utils/loadData.R")
source("utils/extractDates.R")
source("utils/dataManagement.R")
source("RFfunction.R")

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"


# Link to landsat gpkg of wur change dataset 
linkRawValidation = "../data/raw/WURChange20152019_Landsat8_TS.gpkg"
nameBands <- st_layers(linkRawValidation)

# Read in b2 to filter
b2 = st_read(linkRawValidation, nameBands$name[2])
st_geometry(b2)=NULL

# change colnames dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))
colnames(b2)[4:194] = NewColDates

# Filter blue band
b2Filtered = filterBands(b2, smoothLoessPlot, dates)

# Apply on other bands
b1 <- st_read(linkRawValidation, nameBands$name[1])
b3 <- st_read(linkRawValidation, nameBands$name[3])
b4 <- st_read(linkRawValidation, nameBands$name[4])
b5 <- st_read(linkRawValidation, nameBands$name[5])
b6 <- st_read(linkRawValidation, nameBands$name[6])
b7 <- st_read(linkRawValidation, nameBands$name[7])
st_geometry(b1) = NULL
st_geometry(b3) = NULL
st_geometry(b4) = NULL
st_geometry(b5) = NULL
st_geometry(b6) = NULL
st_geometry(b7) = NULL
colnames(b1)[4:194] = NewColDates
colnames(b3)[4:194] = NewColDates
colnames(b4)[4:194] = NewColDates
colnames(b5)[4:194] = NewColDates
colnames(b6)[4:194] = NewColDates
colnames(b7)[4:194] = NewColDates

mean(is.na(b2))
mean(is.na(b2Filtered))
 
# nothing saved from this, need to rerun the filter on b2 next time

# Apply b2 filter to other bands
b2Matrix = as.matrix(b2Filtered[,NewColDates])

b7Filtered = b7
temp = as.matrix(b7Filtered)[,NewColDates]
temp[is.na(b2Matrix)] = NA

b7Filtered[,NewColDates] = temp
mean(is.na(b7[,NewColDates]))
mean(is.na(b7Filtered[,NewColDates]))
mean(is.na(b2Filtered))

b1FilteredSF <- DFtoSF(b1Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b2Filtered = cbind("location_id" = b2$location_id,
                   "sample_x" = b2$sample_x,
                   "sample_y" = b2$sample_y, b2Filtered)
b2FilteredSF <- DFtoSF(b2Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b3FilteredSF <- DFtoSF(b3Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b4FilteredSF <- DFtoSF(b4Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b5FilteredSF <- DFtoSF(b5Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b6FilteredSF <- DFtoSF(b6Filtered, coords = c("sample_x","sample_y"), validation = TRUE)
b7FilteredSF <- DFtoSF(b7Filtered, coords = c("sample_x","sample_y"), validation = TRUE)

# Save as one gpkg with mulitple layers
st_write(b1FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"processed/WURchangeFiltered.gpkg"), "b7")

## Calc VIs
# Convert to numeric
b4Filtered = as.data.frame(sapply(b4Filtered[,NewColDates], as.numeric))
b5Filtered = as.data.frame(sapply(b5Filtered[,NewColDates], as.numeric))

# Calculate NDVI
ndvi = (b5Filtered - b4Filtered) / (b5Filtered + b4Filtered)
plot(as.numeric(apply(ndvi, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndvi")

# Save ndvi as gpkg
temp = cbind("location_id" = b1Filtered$location_id,
             "sample_x" = b1Filtered$sample_x,
             "sample_y" = b1Filtered$sample_y,
             ndvi)
ndviSF <- DFtoSF(temp, coords = c("sample_x","sample_y"), validation = TRUE) # first source
st_write(ndviSF, "../data/processed/WURchangeVIs.gpkg", "NDVI")

##
# Get temporal hrmonic metrics

# Apply function to get the harmonics of NDVI
test = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b1Filtered[, c("location_id","sample_x", "sample_y")], test)

# Change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save harmonics 
HarmMetricsSF <- DFtoSF(HarmMetrics, coords = c("sample_x","sample_y"), validation = TRUE)
st_write(HarmMetricsSF, "../data/processed/WURchangeHarmonics.gpkg", "NDVI")


HarmMetrics = st_read("../data/processed/WURchangeHarmonics.gpkg", "NDVI")
wurChangeCSV = read.csv("../data/raw/reference_global_100m_orig&change_year2015-2019_20210407.csv")
sum(wurChangeCSV$location_id %in% HarmMetrics$location_id)
sum(HarmMetrics$location_id %in% wurChangeCSV$location_id)
change2015 = subset(wurChangeCSV, dataYear=="2015")
change2016 = subset(wurChangeCSV, dataYear=="2016")
change2017 = subset(wurChangeCSV, dataYear=="2017")
change2018= subset(wurChangeCSV, dataYear=="2018")

dataVali = loadChangeValidationData()
loadChangeValidationData()

### 
# Now split time series into yearly datasets
b4 = st_read(paste0(linkData, "processed/WURchangeFiltered.gpkg"), "b4")
b5 = st_read(paste0(linkData, "processed/WURchangeFiltered.gpkg"), "b5")
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
dates = extractDates()
dates = dates[grepl("2014|2015|2016",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b4[, c("location_id","sample_x", "sample_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2015 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("sample_x","sample_y"), validation = TRUE)
st_write(temp, "../data/processed/2015/WURchangeHarmonics.gpkg", "NDVI")

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
dates = extractDates()
dates = dates[grepl("2015|2016|2017",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b4[, c("location_id","sample_x", "sample_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2016 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("sample_x","sample_y"), validation = TRUE)
st_write(temp, "../data/processed/2016/WURchangeHarmonics.gpkg", "NDVI")

# 2017 #
# 69 observations / dates
b42017 = b4[,colnames(b4)[grepl("2016|2017|2018", colnames(b4))]]
b52017 = b5[,colnames(b5)[grepl("2016|2017|2018", colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b42017, as.numeric))
b5Temp = as.data.frame(sapply(b52017, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates = extractDates()
dates = dates[grepl("2016|2017|2018",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b4[, c("location_id","sample_x", "sample_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2017 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("sample_x","sample_y"), validation = TRUE)
st_write(temp, "../data/processed/2017/WURchangeHarmonics.gpkg", "NDVI")

# 2018 #
# 68 observations / dates
b42018 = b4[,colnames(b4)[grepl("2017|2018|2019", colnames(b4))]]
b52018 = b5[,colnames(b5)[grepl("2017|2018|2019", colnames(b5))]]

# calc ndvi
b4Temp = as.data.frame(sapply(b42018, as.numeric))
b5Temp = as.data.frame(sapply(b52018, as.numeric))
ndvi = (b5Temp - b4Temp) / (b5Temp + b4Temp)
rm(b4Temp)
rm(b5Temp)

# calc temporal harmonics
dates = extractDates()
dates = dates[grepl("2017|2018|2019",dates)]
HarmMetrics = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b4[, c("location_id","sample_x", "sample_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2018 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("sample_x","sample_y"), validation = TRUE)
st_write(temp, "../data/processed/2018/WURchangeHarmonics.gpkg", "NDVI")


##
# Try RF and validate on change data
dataTrain = loadTrainingData()
change2015 = loadChangeValidationData(year = "2015")
change2016 = loadChangeValidationData(year = "2016")
change2017 = loadChangeValidationData(year = "2017")
change2018 = loadChangeValidationData(year = "2018")
temp = subset(change2015, location_id %in% change2016$location_id)
temp = subset(temp, location_id %in% change2017$location_id)
temp = subset(temp, location_id %in% change2018$location_id)
change2015 = subset(change2015, sample_id %in% temp$sample_id)
change2016 = subset(change2016, sample_id %in% temp$sample_id)
change2017 = subset(change2017, sample_id %in% temp$sample_id)
change2018 = subset(change2018, sample_id %in% temp$sample_id)
years = list("2015" = change2015,
             "2016" = change2016,
             "2017" = change2017,
             "2018" = change2018)

# Apply RF on wur's change dataset as validation
listDFs = runRandomForest(train=dataTrain, years=years, PredictType="quantiles")
pred2015 = listDFs[[1]]
pred2016 = listDFs[[2]]
pred2017 = listDFs[[3]]
pred2018 = listDFs[[4]]

write.csv(pred2015, "../data/output/wurChange/predictions-2015-ndvi-median.csv", row.names = F)
write.csv(pred2016, "../data/output/wurChange/predictions-2016-ndvi-median.csv", row.names = F)
write.csv(pred2017, "../data/output/wurChange/predictions-2017-ndvi-median.csv", row.names = F)
write.csv(pred2018, "../data/output/wurChange/predictions-2018-ndvi-median.csv", row.names = F)

stats2015 = getStats(basic2015, change2015)
stats2016 = getStats(basic2016, change2016)
stats2017 = getStats(basic2017, change2017)
stats2018 = getStats(basic2018, change2018)

rmse = data.frame(matrix(ncol=length(classes), nrow=4))
colnames(rmse)=classes
rownames(rmse)=c("2015","2016","2017","2018")
rmse["2015",]=stats2015["RMSE",]
rmse["2016",]=stats2016["RMSE",]
rmse["2017",]=stats2017["RMSE",]
rmse["2018",]=stats2018["RMSE",]
rmse2015 = round(sqrt(mean(unlist(pred2015 - change2015[,classes])^2)), digits = 1)
rmse2016 = round(sqrt(mean(unlist(pred2016 - change2016[,classes])^2)), digits = 1)
rmse2017 = round(sqrt(mean(unlist(pred2017 - change2017[,classes])^2)), digits = 1)
rmse2018 = round(sqrt(mean(unlist(pred2018 - change2018[,classes])^2)), digits = 1)
rmse$overall = c(rmse2015, rmse2016, rmse2017, rmse2018)

mae = data.frame(matrix(ncol=length(classes), nrow=4))
colnames(mae)=classes
rownames(mae)=c("2015","2016","2017","2018")
mae["2015",]=stats2015["MAE",]
mae["2016",]=stats2016["MAE",]
mae["2017",]=stats2017["MAE",]
mae["2018",]=stats2018["MAE",]
mae2015 = round(mean(abs(unlist(pred2015  - change2015[,classes]))), digits = 1)
mae2016 = round(mean(abs(unlist(pred2016  - change2016[,classes]))), digits = 1)
mae2017 = round(mean(abs(unlist(pred2017  - change2017[,classes]))), digits = 1)
mae2018 = round(mean(abs(unlist(pred2018  - change2018[,classes]))), digits = 1)
mae$overall = c(mae2015, mae2016, mae2017, mae2018)
