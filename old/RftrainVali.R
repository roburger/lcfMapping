# MSc Thesis
# 21/01/2021
# Use RF on training and validation set

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Access libraries
library(sf)
library(ranger)

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

# Source function load data
source("utils/loadData.R")
dataTrain = loadTrainingData()
dataVali = loadValidationData()

# Get covars names in c() format
names(dataTrain)
names(dataVali)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
covars = c("x","y","min","max","intercept","co","si","co2","si2","trend",
           "phase1","amplitude1","phase2","amplitude2")
subformula = paste0("~", paste(covars, collapse = "+"))

# create empty df for predictions
predictions = data.frame(matrix(ncol=length(classes), nrow=nrow(dataVali)))
colnames(predictions)=classes

for (class in classes){
  print(class)
  formula = paste0(class, subformula)
  
  rfmodel = ranger(formula, dataTrain)
  output = predict(rfmodel, dataVali[,covars])$predictions
  predictions[,class]=output
  #if (class=="shrub"){break}
}
# scale predictions
testsScale = predictions / rowSums(predictions, na.rm=T) * 100

# save scaled predictions in CSV file
write.csv(testsScale, "../data/output/predictions-ndvi.csv", row.names = F)
predictions= read.csv("../data/output/predictions-ndvi.csv")

# explore predictions
statistics = data.frame(matrix(ncol=length(classes), nrow=2))
colnames(statistics)=classes
rownames(statistics)=c("RMSE","MAE")

for (class in classes){
  # RMSE
  RMSE = sqrt(mean((predictions[[class]]-dataVali[[class]])^2))
  statistics["RMSE",class] = RMSE
  # MAE
  MAE = mean(abs(predictions[[class]]-dataVali[[class]]))
  statistics["MAE",class] = MAE
}

statsRounded = round(statistics, digits = 1)


# visualise dominant class
domClass = data.frame("dominant"=factor(classes[apply(predictions, 1, which.max)]))
domClass = cbind(dataVali[,c("x","y")], domClass)
source("utils/dataManagement.R")
domClassSF = DFtoSF(domClass)
plot(domClassSF["dominant"], pch=20, cex=0.01, key.pos=4, axes=T)
# arrange colors per land class
domClassSF$color = "black"
domClassSF$color[domClassSF$dominant=="bare"]="gray"
domClassSF$color[domClassSF$dominant=="crops"]="pink"
domClassSF$color[domClassSF$dominant=="grassland"]="yellow"
domClassSF$color[domClassSF$dominant=="shrub"]="orange"
domClassSF$color[domClassSF$dominant=="tree"]="darkgreen"
domClassSF$color[domClassSF$dominant=="urban_built_up"]="red"
domClassSF$color[domClassSF$dominant=="water"]="blue"
plot(domClassSF["dominant"], pch=20, cex=0.01, key.pos=4, axes=T, col=domClassSF$color)

domClassSF$color[domClassSF$dominant=="bare"]="#dcdcdc"
domClassSF$color[domClassSF$dominant=="crops"]="#f096ff"
domClassSF$color[domClassSF$dominant=="grassland"]="#ffff4c"
domClassSF$color[domClassSF$dominant=="shrub"]="#ffbb22"
domClassSF$color[domClassSF$dominant=="tree"]="#09cc00"
domClassSF$color[domClassSF$dominant=="urban_built_up"]="#ff0000"
domClassSF$color[domClassSF$dominant=="water"]="#1919ff"
namecolor = c("#dcdcdc","#f096ff","#ffff4c","#ffbb22","#09cc00","#ff0000","#1919ff")
plot(domClassSF["dominant"], pch=20, cex=0.01, key.pos=4, axes=F, col=domClassSF$color)
#plot(domClassSF["dominant"], pch=20, cex=0.01, key.pos=1, axes=F, pal=namecolor)

legend(0,-0.6, legend = c("bare","crops","grassland","shrub","tree","urban","water"),
       col = c("#dcdcdc","#f096ff","#ffff4c","#ffbb22","#09cc00","#ff0000","#1919ff"), 
       pch=20, horiz=T, cex=0.5)

# old below
rfmodel = ranger(dataTrain$tree ~ ., importance = "impurity",
                 data=dataTrain[!names(dataTrain) %in% c("tree","location_id","x","y")])
output = predict(rfmodel, testX[!names(testX) %in% c("location_id","x","y")])





# -------------------- #
## Load in train and vali data (csv & gpkg) ##
# ...

# TrainX
harmonics = st_read(paste0(linkData, "processed/IIASAtrainingHarmonics.gpkg"), "NDVI")
st_geometry(harmonics)=NULL
locationID = read.csv("../data/processed/IIASAtrainingLocationID.csv")
harmonics = cbind(locationID,harmonics)
rm(locationID)

# TrainY
filename = "../data/raw/training_data_2015_100m_20190402_V4_New.csv" #use this csv!!!
samplePoints = read.csv2(filename, header = T, sep = ",", dec = ".") # creates num columns
samplePoints$ï..rowid = NULL #remove duplicate ID column
rm(filename)


# Remove rows that have NA in some key columns. TODO: make function of this
Before = nrow(harmonics)
dropRows = apply(harmonics, 1, function(x){any(!is.finite(x))})
harmonics = harmonics[!dropRows,]
After = nrow(harmonics)
print(paste("Dropped NAs, data frame size reduced from", Before, "to", After)) # 150.405 -> 149.775
samplePoints = subset(samplePoints, location_id %in% harmonics$location_id)
all(samplePoints$location_id %in% harmonics$location_id)
all(harmonics$location_id %in% samplePoints$location_id)

# Update dominant lc first
table(samplePoints$dominant_lc)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
DominantClasses = apply(classFractions, 1, which.max)
classes[DominantClasses]
samplePoints$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
sum(classes[DominantClasses] == samplePoints$dominant_lc)

# Drop rows dominated by not_sure
samplePoints = samplePoints[!samplePoints$dominant_lc == "not_sure",]

# Merge classes
ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
             wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")

for (class in 1:length(ClassMap)){
  print(class)
  if (names(ClassMap[class]) %in% names(samplePoints))
    samplePoints[[ClassMap[class]]] = samplePoints[[ClassMap[class]]] + samplePoints[[names(ClassMap[class])]]
  print("done")
}

classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")

sum(rowSums(samplePoints[, classes]) != 100) # many rows not valid 5.269 (4.661)

sum(rowSums(samplePoints[, classes]) == 0) # -> 5.048 (4.440) rows with zero fractions

# Remove rows with Zero fractions
RelevantClasses = samplePoints[, classes]
ClassSums = rowSums(RelevantClasses)
ZeroRows = ClassSums == 0

if (any(ZeroRows)){
  print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
  RelevantClasses = RelevantClasses[!ZeroRows,]
  samplePoints = samplePoints[!ZeroRows,]
  ClassSums = ClassSums[!ZeroRows]
}
sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (221)
sum(rowSums(samplePoints[, classes]) == 0) # -> 0 rows with zero fractions

# Rescale classes to 100%
samplePoints[,classes] = RelevantClasses / (ClassSums / 100)
sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (only 35?)
sum(round(rowSums(samplePoints[, classes])) != 100) # now 0 rows with round!

# Update dominant class
classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
DominantClasses = apply(classFractions, 1, which.max)
classes[DominantClasses]
samplePoints$dominant_lc = factor(classes[DominantClasses])
rm(classFractions)
rm(RelevantClasses)

# Remove old columns
samplePoints$burnt=NULL
samplePoints$fallow_shifting_cultivation=NULL
samplePoints$lichen_and_moss=NULL
samplePoints$snow_and_ice=NULL
samplePoints$wetland_herbaceous=NULL
samplePoints$not_sure=NULL

# done merging land cover classes and rescaling to 100% for train and vali
# use same points for harmonics (trainY) with subset
harmonics = subset(harmonics, location_id %in% samplePoints$location_id)

# Merge TrainX and TrainY
#merge(samplePoints, harmonics) # this merge leads to a loss of 69 points (mistake in round numerical values)
dataTrain = merge(samplePoints, harmonics, by="location_id") # this does not result in loss of points, but adds new columns for coords
dataTrain[which(!(dataTrain$y.x == dataTrain$y.y)),c("location_id","x.x","x.y","y.x","y.y")]
# so new coords are still same, but not recognised as identical (different digits)
# Change column names and remove duplicate coords columns
names(dataTrain)[names(dataTrain) == "x.x"] = "x"
names(dataTrain)[names(dataTrain) == "y.x"] = "y"
dataTrain$x.y=NULL
dataTrain$y.y=NULL



## ----------------- ##
# now do the same for Validation set 
# Put vali code here  


## ----------------- ##
# ValiX
filename = "../data/raw/refdata_world_africa_included_locations_data20190709.csv"
validationRaw = read.csv(filename, header = T)
rm(filename)
names(validationRaw)[names(validationRaw) == "subpix_mean_x"] = "x"
names(validationRaw)[names(validationRaw) == "subpix_mean_y"] = "y"
names(validationRaw)[names(validationRaw) == "ï..sample_id"] = "sample_id"

# ValiY
HarmMetrics = st_read(paste0(linkData, "processed/WURvalidationHarmonics.gpkg"), "NDVI")
st_geometry(HarmMetrics)=NULL
names(HarmMetrics)[names(HarmMetrics) == "subpix_mean_x"] = "x"
names(HarmMetrics)[names(HarmMetrics) == "subpix_mean_y"] = "y"

# So apparantly training and validation csv's have different land cover classes
# Need to be the same before applying RF on both train and vali....
# Check how to match classes in github code
# utils -> covariate names -> getcommonclasnames()
# it is probably in TidyData
# it first recalculates the most dominant class
# then dropping those that are 'not_sure'
# then reclassify and update most dominant again

# Check which classes there are present
# Training
colnames(samplePoints)
# 1) bare 2) burnt 3) crops 4) fallow shifting 5) grassland 5) lichen moss
# 6) shrub 7) snow ice 8) tree 9) urban 10) water 11) wetland herbs 12) not sure

# Validation
colnames(validationRaw)
# 1) tree 2) grass 3) shrub 4) crops 5) urban 6) bare 7) lichen 8) water
# 9) snow 10) fl.grass 11) fl.lichen

## now testing of TidyData ##
# check nr of NA's per column
apply(HarmMetrics, 2, function(x){sum(is.na(x))}) / nrow(harmonics) * 100

# Remove rows that have NA's in column
# now testing on validation set only
Before = nrow(HarmMetrics)
dropRows = apply(HarmMetrics, 1, function(x){any(!is.finite(x))})
HarmMetrics = HarmMetrics[!dropRows,]
After = nrow(HarmMetrics)
print(paste("Dropped NAs, data frame size reduced from", Before, "to", After)) # 21.752 -> 21.705
validationRaw = subset(validationRaw, sample_id %in% HarmMetrics$sample_id)
all(validationRaw$sample_id %in% HarmMetrics$sample_id)
all(HarmMetrics$sample_id %in% validationRaw$sample_id)

# TODO: now update dominant class
# first rename colnames to common colnames
names(validationRaw)[names(validationRaw) == "trees"] = "tree"
names(validationRaw)[names(validationRaw) == "grass"] = "grassland"
names(validationRaw)[names(validationRaw) == "urban"] = "urban_built_up"
names(validationRaw)[names(validationRaw) == "wetland"] = "wetland_herbaceous" # no wetland?

classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
classFractions = validationRaw[,classes[classes %in% names(validationRaw)]]
DominantClasses = apply(classFractions, 1, which.max)
classes[DominantClasses]
validationRaw$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover

# Now Reclassify and merge classes
# ..
#ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
#     wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")
ClassMap = c(lichen="grassland", snow="bare", fl.grass="grassland", fl.lichen="grassland")

for (class in 1:length(ClassMap)){
  print(class)
  if (names(ClassMap[class]) %in% names(validationRaw))
    validationRaw[[ClassMap[class]]] = validationRaw[[ClassMap[class]]] + validationRaw[[names(ClassMap[class])]]
  print("done")
  }

sum(rowSums(validationRaw[, classes]) != 100)
sum(rowSums(validationRaw[, classes]) == 0)
# TidyData -> ReclassifyAndScale -> nog afmaken vanaf line 130
# waarschijnlijk niet nodig vanwege alles al 100 is..
# dus nog doen: keep only the 7 land cover classes, and remove other columns
validationRaw$lichen = NULL
validationRaw$snow = NULL
validationRaw$fl.grass = NULL
validationRaw$fl.lichen = NULL

# Update dominant land cover class
table(validationRaw$dominant_lc)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")
classFractions = validationRaw[,classes[classes %in% names(validationRaw)]]
DominantClasses = apply(classFractions, 1, which.max)
#classes[DominantClasses]
validationRaw$dominant_lc = factor(classes[DominantClasses])
table(validationRaw$dominant_lc)
rm(classFractions)

# Merge valiRaw with Features
#dataVali = merge(validationRaw, HarmMetrics)
dataVali = merge(validationRaw, HarmMetrics, by="sample_id")
dataVali[which(!(dataVali$y.x == dataVali$y.y)),c("sample_id","x.x","x.y","y.x","y.y")]
# seems like the same problem, still same coords but not identical.
# however, now this is happening to almost every row
# double-check this with rounding the coords colums -> DONE: still round problem
# Change column names and remove duplicate coords columns
names(dataVali)[names(dataVali) == "x.x"] = "x"
names(dataVali)[names(dataVali) == "y.x"] = "y"
dataVali$x.y=NULL
dataVali$y.y=NULL



## Now merge classes for training data ##

# Remove rows that have NA's in column (not used now)
# now testing on training set
Before = nrow(harmonics)
dropRows = apply(harmonics, 1, function(x){any(!is.finite(x))})
test = harmonics[!dropRows,]
After = nrow(test)
print(paste("Dropped NAs, data frame size reduced from", Before, "to", After))
# 150.405 -> 149.775

# Update dominant lc first
table(samplePoints$dominant_lc)
classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water",
            "burnt", "fallow_shifting_cultivation", "wetland_herbaceous", "lichen_and_moss",
            "snow_and_ice", "not_sure")
classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
DominantClasses = apply(classFractions, 1, which.max)
classes[DominantClasses]
samplePoints$dominant_lc = factor(classes[DominantClasses]) # new column with dominant land cover
sum(classes[DominantClasses] == samplePoints$dominant_lc)

# Drop rows dominated by not_sure
samplePoints = samplePoints[!samplePoints$dominant_lc == "not_sure",]

# Merge classes
ClassMap = c(burnt="grassland", fallow_shifting_cultivation="crops", 
             wetland_herbaceous="grassland", lichen_and_moss="grassland", snow_and_ice="bare")

for (class in 1:length(ClassMap)){
  print(class)
  if (names(ClassMap[class]) %in% names(samplePoints))
    samplePoints[[ClassMap[class]]] = samplePoints[[ClassMap[class]]] + samplePoints[[names(ClassMap[class])]]
  print("done")
}

classes = c("tree", "shrub", "grassland", "crops", "urban_built_up", "bare", "water")

sum(rowSums(samplePoints[, classes]) != 100) # many rows not valid (5.269)

sum(rowSums(samplePoints[, classes]) == 0) # -> (5.048) rows with zero fractions

# Remove rows with Zero fractions
RelevantClasses = samplePoints[, classes]
ClassSums = rowSums(RelevantClasses)
ZeroRows = ClassSums == 0

if (any(ZeroRows)){
  print(paste("Dropping", sum(ZeroRows), "samples because all their relevant fractions are zero"))
  RelevantClasses = RelevantClasses[!ZeroRows,]
  samplePoints = samplePoints[!ZeroRows,]
  ClassSums = ClassSums[!ZeroRows]
}
sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (221)
sum(rowSums(samplePoints[, classes]) == 0) # -> 0 rows with zero fractions

# Rescale classes to 100%
test = samplePoints
samplePoints[,classes] = RelevantClasses / (ClassSums / 100)
sum(rowSums(samplePoints[, classes]) != 100) # still some rows not valid (only 35?)
sum(round(rowSums(samplePoints[, classes])) != 100) # now 0 rows with round!

# Update dominant class
classFractions = samplePoints[,classes[classes %in% names(samplePoints)]]
DominantClasses = apply(classFractions, 1, which.max)
classes[DominantClasses]
samplePoints$dominant_lc = factor(classes[DominantClasses])

# Remove old columns
samplePoints$burnt=NULL
samplePoints$fallow_shifting_cultivation=NULL
samplePoints$lichen_and_moss=NULL
samplePoints$snow_and_ice=NULL
samplePoints$wetland_herbaceous=NULL
samplePoints$not_sure=NULL

# done merging land cover classes and rescaling to 100% for train and vali
# use same points for harmonics (trainY) with subset
harmonics = subset(harmonics, location_id %in% samplePoints$location_id)
