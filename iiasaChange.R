# MSc Thesis
# 20/02/2021
# Process IIASA change data

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

source("RFfunctionNew.R")

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

# Link to landsat gpkg of wur change dataset 
linkRawData = "../data/raw/IIASAChange20152018_Landsat8_TS.gpkg"
nameBands <- st_layers(linkRawData)


# Read in b2 to filter
b2 = st_read(linkRawData, nameBands$name[2])
st_geometry(b2)=NULL

# change colnames dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))
colnames(b2)[4:194] = NewColDates

# Filter blue band
b2Filtered = filterBands(b2, smoothLoessPlot, dates)


# Apply on other bands
b1 <- st_read(linkRawData, nameBands$name[1])
b3 <- st_read(linkRawData, nameBands$name[3])
b4 <- st_read(linkRawData, nameBands$name[4])
b5 <- st_read(linkRawData, nameBands$name[5])
b6 <- st_read(linkRawData, nameBands$name[6])
b7 <- st_read(linkRawData, nameBands$name[7])
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


# Apply b2 filter to other bands
b2Matrix = as.matrix(b2Filtered[,NewColDates])

b7Filtered = b7
temp = as.matrix(b7Filtered)[,NewColDates]
temp[is.na(b2Matrix)] = NA

b7Filtered[,NewColDates] = temp
mean(is.na(b7[,NewColDates]))
mean(is.na(b7Filtered[,NewColDates]))
mean(is.na(b2Filtered))

# Store filtered bands, transform to SF to save in gpkg
b1FilteredSF <- DFtoSF(b1Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b2Filtered = cbind("sample_id" = b2$sample_id,
                   "centroid_x" = b2$centroid_x,
                   "centroid_y" = b2$centroid_y, b2Filtered)
b2FilteredSF <- DFtoSF(b2Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b3FilteredSF <- DFtoSF(b3Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b4FilteredSF <- DFtoSF(b4Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b5FilteredSF <- DFtoSF(b5Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b6FilteredSF <- DFtoSF(b6Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)
b7FilteredSF <- DFtoSF(b7Filtered, coords = c("centroid_x","centroid_y"), validation = TRUE)


# Save as one gpkg with mulitple layers
st_write(b1FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b1")
st_write(b2FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b2")
st_write(b3FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b3")
st_write(b4FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b4")
st_write(b5FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b5")
st_write(b6FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b6")
st_write(b7FilteredSF, paste0(linkData,"processed/IIASAchangeFiltered.gpkg"), "b7")


## Calc VIs
# Convert to numeric
b4Filtered = as.data.frame(sapply(b4Filtered[,NewColDates], as.numeric))
b5Filtered = as.data.frame(sapply(b5Filtered[,NewColDates], as.numeric))

# Calculate NDVI
ndvi = (b5Filtered - b4Filtered) / (b5Filtered + b4Filtered)
plot(as.numeric(apply(ndvi, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndvi")

# Save ndvi as gpkg
temp = cbind("sample_id" = b1Filtered$sample_id,
             "centroid_x" = b1Filtered$centroid_x,
             "centroid_y" = b1Filtered$centroid_y,
             ndvi)
ndviSF <- DFtoSF(temp, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(ndviSF, "../data/processed/IIASAchangeVIs.gpkg", "NDVI")


##
# Get temporal hrmonic metrics

# Apply function to get the harmonics of NDVI
test = t(pbapply(as.matrix(ndvi), 1, getHarmonics))
HarmMetrics = cbind(b1Filtered[, c("sample_id","centroid_x", "centroid_y")], test)


# Change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save harmonics 
HarmMetricsSF <- DFtoSF(HarmMetrics, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(HarmMetricsSF, "../data/processed/IIASAchangeHarmonics.gpkg", "NDVI")


iiasaChangeCSV = read.csv("../data/raw/Data_Global_quoted.csv")
sum(iiasaChangeCSV$sample_id %in% HarmMetrics$sample_id)
sum(HarmMetrics$sample_id %in% iiasaChangeCSV$sample_id)

# Now split time series into yearly datasets
b4 = st_read(paste0(linkData, "processed/IIASAchangeFiltered.gpkg"), "b4")
b5 = st_read(paste0(linkData, "processed/IIASAchangeFiltered.gpkg"), "b5")
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
HarmMetrics = cbind(b4[, c("sample_id","centroid_x", "centroid_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2015 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(temp, "../data/processed/2015/IIASAchangeHarmonics.gpkg", "NDVI")

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
HarmMetrics = cbind(b4[, c("sample_id","centroid_x", "centroid_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2016 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(temp, "../data/processed/2016/IIASAchangeHarmonics.gpkg", "NDVI")

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
HarmMetrics = cbind(b4[, c("sample_id","centroid_x", "centroid_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2017 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(temp, "../data/processed/2017/IIASAchangeHarmonics.gpkg", "NDVI")

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
HarmMetrics = cbind(b4[, c("sample_id","centroid_x", "centroid_y")], HarmMetrics)

# change colnames
names(HarmMetrics)[4:(length(HarmMetrics))]= c("min", "max", "intercept", "co", 
                                               "si", "co2", "si2", "trend", "phase1", 
                                               "amplitude1", "phase2", "amplitude2")
names(HarmMetrics)

# Save (ndvi) 2018 features as GPKG
temp = DFtoSF(HarmMetrics, coords = c("centroid_x","centroid_y"), validation = TRUE)
st_write(temp, "../data/processed/2018/IIASAchangeHarmonics.gpkg", "NDVI")


# Stored yearly features for iiasa change set
# TODO: implement iiasa change in loadData functions -> done
# Load all training and vali data
dataTrain = loadTrainingData()
train2015 = loadChangeTrainingData("2015")
train2016 = loadChangeTrainingData("2016")
train2017 = loadChangeTrainingData("2017")
train2018 = loadChangeTrainingData("2018")
temp = subset(train2015, sample_id %in% train2016$sample_id)
temp = subset(temp, sample_id %in% train2017$sample_id)
temp = subset(temp, sample_id %in% train2018$sample_id)
train2015 = subset(train2015, sample_id %in% temp$sample_id)
train2016 = subset(train2016, sample_id %in% temp$sample_id)
train2017 = subset(train2017, sample_id %in% temp$sample_id)
train2018 = subset(train2018, sample_id %in% temp$sample_id)
listChange = list("2015" = train2015,
                 "2016" = train2016,
                 "2017" = train2017,
                 "2018" = train2018)
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



listDFs = runRecurrentRF(train=dataTrain, years=years,
                         features=loadFeaturesNames(), PredictType="quantiles")
rec2015 = listDFs[[1]]
rec2016 = listDFs[[2]]
rec2017 = listDFs[[3]]
rec2018 = listDFs[[4]]

write.csv(rec2015, "../data/output/wurChange/predictions-2015-median-recurrent.csv", row.names = F)
write.csv(rec2016, "../data/output/wurChange/predictions-2016-median-recurrent.csv", row.names = F)
write.csv(rec2017, "../data/output/wurChange/predictions-2017-median-recurrent.csv", row.names = F)
write.csv(rec2018, "../data/output/wurChange/predictions-2018-median-recurrent.csv", row.names = F)

# This recurrent model works and functions
# TODO: move to RFfunction.R
# TODO: check basic RF, rmse and mae, and save in paint, check with RMSEandMAE file
basic2015 = read.csv("../data/output/wurChange/predictions-2015-median.csv")
basic2016 = read.csv("../data/output/wurChange/predictions-2016-median.csv")
basic2017 = read.csv("../data/output/wurChange/predictions-2017-median.csv")
basic2018 = read.csv("../data/output/wurChange/predictions-2018-median.csv")

markov2015 = read.csv("../data/output/markov/smooth2015-basic.csv")
markov2016 = read.csv("../data/output/markov/smooth2016-basic.csv")
markov2017 = read.csv("../data/output/markov/smooth2017-basic.csv")
markov2018 = read.csv("../data/output/markov/smooth2018-basic.csv")

rec2015 = read.csv("../data/output/wurChange/predictions-2015-median-recurrent.csv")
rec2016 = read.csv("../data/output/wurChange/predictions-2016-median-recurrent.csv")
rec2017 = read.csv("../data/output/wurChange/predictions-2017-median-recurrent.csv")
rec2018 = read.csv("../data/output/wurChange/predictions-2018-median-recurrent.csv")

basicRMSE2015 = round(sqrt(mean(unlist(basic2015 - change2015[,classes])^2)), digits = 1)
basicRMSE2016 = round(sqrt(mean(unlist(basic2016 - change2016[,classes])^2)), digits = 1)
basicRMSE2017 = round(sqrt(mean(unlist(basic2017 - change2017[,classes])^2)), digits = 1)
basicRMSE2018 = round(sqrt(mean(unlist(basic2018 - change2018[,classes])^2)), digits = 1)

basicMAE2015 = round(mean(abs(unlist(basic2015  - change2015[,classes]))), digits = 1)
basicMAE2016 = round(mean(abs(unlist(basic2016  - change2016[,classes]))), digits = 1)
basicMAE2017 = round(mean(abs(unlist(basic2017  - change2017[,classes]))), digits = 1)
basicMAE2018 = round(mean(abs(unlist(basic2018  - change2018[,classes]))), digits = 1)

# basic rmse and mae correspond with paint img (RMSEandMAE)
# recheck basic RF-model if it works well, and rerun to recheck stats
listDFs = runRandomForest(years=years, PredictType="quantiles")
pred2015 = listDFs[[1]]
pred2016 = listDFs[[2]]
pred2017 = listDFs[[3]]
pred2018 = listDFs[[4]]

RMSE2015 = round(sqrt(mean(unlist(pred2015 - change2015[,classes])^2)), digits = 1)
RMSE2016 = round(sqrt(mean(unlist(pred2016 - change2016[,classes])^2)), digits = 1)
RMSE2017 = round(sqrt(mean(unlist(pred2017 - change2017[,classes])^2)), digits = 1)
RMSE2018 = round(sqrt(mean(unlist(pred2018 - change2018[,classes])^2)), digits = 1)

MAE2015 = round(mean(abs(unlist(pred2015  - change2015[,classes]))), digits = 1)
MAE2016 = round(mean(abs(unlist(pred2016  - change2016[,classes]))), digits = 1)
MAE2017 = round(mean(abs(unlist(pred2017  - change2017[,classes]))), digits = 1)
MAE2018 = round(mean(abs(unlist(pred2018  - change2018[,classes]))), digits = 1)


## Subpixel confusion matrix
names(pred2018)=classes
s_k = pred2015
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
all(round(rowSums(s_k))==100)
s_k = as.matrix(s_k)

truth = actual2015[,classes]
all(round(rowSums(truth))==100)
r_l = as.matrix(truth)
# tomorrow: continue with line 403 in classifyRF: SCM()
# utils/subpixel-confusion-matrix --> Comparator line 124
# Comparator(): Cumulative result from line 22 and line 31
CumulativeResult = matrix(0, ncol(s_k), ncol(r_l))
nrow(s_k)

MIN_D = function(sp_nk, rp_nl, k, l)
{
  min(sp_nk[k], rp_nl[l])
}
A = match.fun(min)
D = MIN_D




for (n in 1:nrow(s_k))
{
  s_nk = s_k[n,]
  r_nl = r_l[n,]
  
  K = length(s_nk)
  
  # Overestimation and underestimation
  sp_nk = s_nk - r_nl; sp_nk[sp_nk<0] = 0
  rp_nl = r_nl - s_nk; rp_nl[rp_nl<0] = 0
  
  Result = matrix(NA, K, K) #p_nkl

  for (k in 1:K){
    
    for (l in 1:K)
    {
      if (k == l) { # Diagonal
        if (!is.null(A))
          Result[k,l] = A(s_nk[k], r_nl[l])
        else
          Result[k,l] = D(sp_nk, rp_nl, k, l)
      } else {
        if (!is.null(D))
          Result[k,l] = D(sp_nk, rp_nl, k, l)
        else
          Result[k,l] = A(s_nk[k], r_nl[l])
      }
    }
  }
  CumulativeResult = CumulativeResult + Result
  
  rownames(CumulativeResult) = colnames(s_k)
  colnames(CumulativeResult) = colnames(r_l)
}


##
source("utils/subpixelConfusionMatrix.R")
s_k = rec2016
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
truth = change2016[,classes]

SCM(s_k/100, truth/100, plot = FALSE, totals = TRUE)

# Overall Accuracy
#       Basic       Recurrent
# 2015  63.3%±1.1   63.3%±1.0
# 2016  62.5%±1.1   74.8%±0.8
# 2017  61.9%±1.1   74.1%±0.8
# 2018  62.1%±1.1   73.9%±0.8

# Kappa
#       Basic       Recurrent
# 2015  0.52±0.02   0.52±0.02
# 2016  0.51±0.02   0.67±0.01
# 2017  0.50±0.02   0.67±0.01
# 2018  0.51±0.02   0.66±0.01

# maybe nice to check OA and Kappa for wur2015 as validation set
orig2015 = read.csv("../data/output/predictions-2015-ndvi-median.csv")
orig2016 = read.csv("../data/output/predictions-2016-ndvi-median.csv")
orig2017 = read.csv("../data/output/predictions-2017-ndvi-median.csv")
orig2018 = read.csv("../data/output/predictions-2018-ndvi-median.csv")
origRef2015 = loadValidationData("2015")
origRef2016 = loadValidationData("2016")
origRef2017 = loadValidationData("2017")
origRef2018 = loadValidationData("2018")
temp = subset(origRef2015, location_id %in% origRef2016$location_id)
temp = subset(temp, location_id %in% origRef2017$location_id)
temp = subset(temp, location_id %in% origRef2018$location_id)
origRef2015 = subset(origRef2015, sample_id %in% temp$sample_id)
origRef2016 = subset(origRef2016, sample_id %in% temp$sample_id)
origRef2017 = subset(origRef2017, sample_id %in% temp$sample_id)
origRef2018 = subset(origRef2018, sample_id %in% temp$sample_id)

source("utils/subpixelConfusionMatrix.R")
s_k = rec2017
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
truth = change2017[,classes]

SCM(s_k/100, truth/100, plot = FALSE, totals = TRUE)

# Overall Accuracy on change with new features
#       Basic       Recurrent   Markov    Recurrent(IIASA2015+prev)
# 2015  67.9+1.1    68.0+1.2    68.7+1.3  67.9+1.1
# 2016  67.2+1.2    64.4+1.1              67.1+1.1
# 2017  67.0+1.2    60.7+1.0              65.6+1.1
# 2018  67.2+1.3    58.4+1.0              64.8+1.1

# recurrent decreasing OA over the years
# due to:
# maybe prev brings some uncertainty?
# no normalisation of prev each year, brings less acc?

## For validation set of 2015
# Overall Accuracy
#       Basic       Recurrent
# 2015  68.3+1.1    
# 2016  68.2+1.2
# 2017  67.9+1.1
# 2018  67.9+1.1

# Kappa
#       Basic       Recurrent
# 2015  0.58+0.02   
# 2016  0.57+0.02
# 2017  0.57+0.02
# 2018  0.57+0.01





runRecurrentRF <- function(train=loadTrainingData(), change=NULL,
                           years=loadValidationData(), 
                           features=loadNDVIcovarsNames(), PredictType="response"){
  
  # Set median voting settings
  quantreg = ifelse(PredictType=="response", FALSE, TRUE)
  PredictQuantiles = 0.5
  
  # Load class names
  classes = loadClassNames()
  
  # Create initial formula string
  subformula = paste0("~", paste(features, collapse = "+"))
  
  # Create empty df's to store yearly predictions on validation set of change
  for (i in 1:length(years)){
    #print(paste0(i, ": create empty DF"))
    temp = data.frame(matrix(ncol=length(classes), nrow=nrow(years[[i]])))
    colnames(temp)=classes
    assign(paste0("rec", names(years)[i]), temp)
  }
  
  # Create empty df's to store yearly predictions on training change locations
  for (i in 1:length(change)){
    #print(paste0(i, ": create empty DF"))
    temp = data.frame(matrix(ncol=length(classes), nrow=nrow(change[[i]])))
    colnames(temp)=classes
    assign(paste0("predTrain", names(years)[i]), temp)
  }
  
  n = 1
  # Apply RF for each class
  for (class in classes){
    print(paste0("Class (", n, "/", length(classes), "): ", class))
    n = n + 1
    
    # Adjust formula
    formula = paste0(class, subformula)
    
    # Run RF
    rfmodel = ranger(formula, train, seed = 0xbadcafe, quantreg=quantreg)
    
    # Make predictions for every year (2015, 2016, 2017, 2018)
    for (j in 1:length(years)){
      
      # Retrain for 2016, 2017, 2018, to include previous year
      # Not retrain every year
      # Train recurrent model for 2016, based on features of 2016 and 2015 class output 
      # then make predictions for next year
      if (j == 2){
        
        # train.recurrent = train + prev
        #print("adding new col for previous year")
        train.recurrent = data.frame(change[[2]][,c(class,features)], 
                                     "prev" = predTrain2015[[class]]) # maybe CBIND?
        #prevDF = data.frame(years[[1]][,c(class,features)], "prev"=rec2015[[class]])
        #print(paste0("rec2015: ",class(rec2015[[class]])))
        #print(paste0("prevDF: ", class(prevDF)))
        #print(paste0("prevDF$prev: ", class(prevDF$prev)))
        
        #train.recurrent = rbind.fill(train, prevDF)
        
        # Replace NAs with mean
        #print("replacing NAs for the mean")
        
        #CM = mean(train.recurrent[["prev"]], na.rm = T)
        #print(CM)
        #column = train.recurrent$prev
        #column[is.na(column)] = CM
        #train.recurrent$prev = column
        #train.recurrent$prev[is.na(train.recurrent$prev)] = CM
        
        #print("seems to work")
        
        # Adjust formula to include previous year's output
        recFormula = paste0(formula, "+prev")
        
        # Run recurrent RF
        print("Building recurrent model for 2016, 2017, 2018...")
        rfmodel = ranger(recFormula, train.recurrent, seed = 0xbadcafe, quantreg=quantreg,
                         importance = "permutation")
      }
      
      if (j != 1){
        # Include previous year's output in validation set 
        vali = data.frame(years[[j]][,features],years[[j-1]][class])
        names(vali)[length(names(vali))] = "prev"
        
        #vali = data.frame(years[[j]][,features], 
        #                  "prev"= eval(parse(text=paste0("rec",names(years)[j-1])))[[class]])
        trainChange = data.frame(change[[j]][,features],
                                 "prev" = eval(parse(text=paste0("predTrain",names(change)[j-1])))[[class]])
      }
      else {
        vali = data.frame(years[[j]][,features])
        trainChange = change[[j]][,features]
      }
      
      # Apply RF-model to predict on validation set
      print(paste0("Predicting for ", names(years[j]), "..."))
      output = predict(rfmodel, vali,
                       type=PredictType, quantiles=PredictQuantiles)$predictions
      
      # Store class prediction of every year in df
      temp = eval(parse(text = paste0("rec", names(years)[j])))
      temp[,class] = as.numeric(output)
      assign(paste0("rec", names(years)[j]), temp)
      
      # !
      # Also predict on change training locations (input for recurrent model)
      print(paste0("Predicting on change locations for ", names(change[j]), "..."))
      output = predict(rfmodel, trainChange,
                       type=PredictType, quantiles=PredictQuantiles)$predictions
      
      # Store class prediction in df
      temp = eval(parse(text = paste0("predTrain", names(change)[j])))
      temp[,class] = as.numeric(output)
      assign(paste0("predTrain", names(change)[j]), temp)
      
    }
    #if (class=="shrub"){break}
  }
  
  # Scale predictions
  # TODO: loop over predictions DF's of all years and scale
  #predictions = predictions / rowSums(predictions, na.rm=T) * 100
  print("Scaling the predictions...")
  for (i in 1:length(years)){
    #assign(paste0("predictions", names(years)[i]), temp)
    temp = eval(parse(text = paste0("rec", names(years)[i])))
    temp = temp / rowSums(temp, na.rm=T) * 100
    temp = as.matrix(temp)
    temp[is.nan(temp)] = 0
    assign(paste0("rec", names(years)[i]), as.data.frame(temp))
  }
  
  # Return all yearly predicted DF's 
  return(list(rec2015, rec2016, rec2017, rec2018))
}
