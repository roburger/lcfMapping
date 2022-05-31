# MSc Thesis
# 25/02/2021
# Add new features

# Set working directory (for yourself)
setwd("~/Thesis/code/lcfMapping/")

# Access libraries
library(sf)
library(pbapply)
library(probaV)

source("utils/loadData.R")
source("utils/extractDates.R")
source("utils/dataManagement.R")

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

# Get column dates
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))


## SHOULD CALC VIs FOR IIASA.TRAIN, IIASA.CHANGE, WUR.CHANGE

## IIASA.TRAIN
# Read in b4 and b5 (for ndvi stats)
linkIIASAtrain = "../data/processed/IIASAtrainingFiltered.gpkg"
nameBands <- st_layers(linkIIASAtrain)
coords = read.csv("../data/processed/IIASAtrainingCoords.csv")
ID = read.csv("../data/processed/IIASAtrainingLocationID.csv")

b4 <- st_read(linkIIASAtrain, nameBands$name[4])
b5 <- st_read(linkIIASAtrain, nameBands$name[5])
b6 <- st_read(linkIIASAtrain, nameBands$name[6])
b7 <- st_read(linkIIASAtrain, nameBands$name[7])
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

# Convert to numeric
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

# Do on whole time series first
nbr = (b5 - b7) / (b5 + b7)
ndmi = (b5 - b6) / (b5 + b6)
ndvi = (b5 - b4) / (b5 + b4)

nbrmedian = as.numeric(apply(nbr, 1, function(x){median(x, na.rm = TRUE)}))
nbriqr = as.numeric(apply(nbr, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmimedian = as.numeric(apply(ndmi, 1, function(x){median(x, na.rm = TRUE)}))
ndmiiqr = as.numeric(apply(ndmi, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvimedian = as.numeric(apply(ndvi, 1, function(x){median(x, na.rm = TRUE)}))
ndviiqr = as.numeric(apply(ndvi, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(ID, coords,
                     nbrmedian, ndmimedian, ndvimedian,
                     nbriqr, ndmiiqr, ndviiqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015)
st_write(VIs2015SF, "../data/processed/IIASAtrainingVIs.gpkg")


# Split data into 2015, 2016, 2017 and 2018

# 2015
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]]
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]
b62015 = b6[,colnames(b6)[grepl("2014|2015|2016", colnames(b6))]]
b72015 = b7[,colnames(b7)[grepl("2014|2015|2016", colnames(b7))]]

#ndbi2015 = (b62015 - b52015) / (b62015 + b52015)
#plot(as.numeric(apply(ndbi2015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndbi")
nbr2015 = (b52015 - b72015) / (b52015 + b72015)
plot(as.numeric(apply(nbr2015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean nbr")
ndmi2015 = (b52015 - b62015) / (b52015 + b62015)
plot(as.numeric(apply(ndmi2015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean ndmi")
#plot(as.numeric(apply(b72015, 2, function(x){mean(x, na.rm = TRUE)})), ylab="mean b7")
ndvi2015 = (b52015 - b42015) / (b52015 + b42015)


nbr2015median = as.numeric(apply(nbr2015, 1, function(x){median(x, na.rm = TRUE)}))
nbr2015iqr = as.numeric(apply(nbr2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2015median = as.numeric(apply(ndmi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2015iqr = as.numeric(apply(ndmi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2015median = as.numeric(apply(ndvi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2015iqr = as.numeric(apply(ndvi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

# write indices itself (skip)
# nbrSF = DFtoSF(nbr2015)
# st_write(nbrSF, "../data/processed/2015/IIASAtrainingVIs.gpkg", "NBR")
# ndmiSF = DFtoSF(ndmi2015)
# st_write(nbrSF, "../data/processed/2015/IIASAtrainingVIs.gpkg", "NDMI")
# ndviSF = DFtoSF(ndvi2015)
# st_write(nbrSF, "../data/processed/2015/IIASAtrainingVIs.gpkg", "NDVI")

# save median and iqr in one table
VIs2015 = data.frame(ID, 
                     nbr2015median, ndmi2015median, ndvi2015median,
                     nbr2015iqr, ndmi2015iqr, ndvi2015iqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015)
st_write(VIs2015SF, "../data/processed/2015/IIASAtrainingVIs.gpkg")

# 2016
b42016 = b4[,colnames(b4)[grepl("2015|2016|2017", colnames(b4))]]
b52016 = b5[,colnames(b5)[grepl("2015|2016|2017", colnames(b5))]]
b62016 = b6[,colnames(b6)[grepl("2015|2016|2017", colnames(b6))]]
b72016 = b7[,colnames(b7)[grepl("2015|2016|2017", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2016 = (b52016 - b72016) / (b52016 + b72016)
ndmi2016 = (b52016 - b62016) / (b52016 + b62016)
ndvi2016 = (b52016 - b42016) / (b52016 + b42016)

# Median + IQR
nbr2016median = as.numeric(apply(nbr2016, 1, function(x){median(x, na.rm = TRUE)}))
nbr2016iqr = as.numeric(apply(nbr2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2016median = as.numeric(apply(ndmi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2016iqr = as.numeric(apply(ndmi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2016median = as.numeric(apply(ndvi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2016iqr = as.numeric(apply(ndvi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2016 = data.frame(ID, coords,
                     nbr2016median, ndmi2016median, ndvi2016median,
                     nbr2016iqr, ndmi2016iqr, ndvi2016iqr)

# write median and iqr VIs
VIs2016SF = DFtoSF(VIs2016)
st_write(VIs2016SF, "../data/processed/2016/IIASAtrainingVIs.gpkg")

# 2017
b42017 = b4[,colnames(b4)[grepl("2016|2017|2018", colnames(b4))]]
b52017 = b5[,colnames(b5)[grepl("2016|2017|2018", colnames(b5))]]
b62017 = b6[,colnames(b6)[grepl("2016|2017|2018", colnames(b6))]]
b72017 = b7[,colnames(b7)[grepl("2016|2017|2018", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2017 = (b52017 - b72017) / (b52017 + b72017)
ndmi2017 = (b52017 - b62017) / (b52017 + b62017)
ndvi2017 = (b52017 - b42017) / (b52017 + b42017)

# Median + IQR
nbr2017median = as.numeric(apply(nbr2017, 1, function(x){median(x, na.rm = TRUE)}))
nbr2017iqr = as.numeric(apply(nbr2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2017median = as.numeric(apply(ndmi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2017iqr = as.numeric(apply(ndmi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2017median = as.numeric(apply(ndvi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2017iqr = as.numeric(apply(ndvi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2017 = data.frame(ID, coords,
                     nbr2017median, ndmi2017median, ndvi2017median,
                     nbr2017iqr, ndmi2017iqr, ndvi2017iqr)

# write median and iqr VIs
VIs2017SF = DFtoSF(VIs2017)
st_write(VIs2017SF, "../data/processed/2017/IIASAtrainingVIs.gpkg")

# 2018
b42018 = b4[,colnames(b4)[grepl("2017|2018|2019", colnames(b4))]]
b52018 = b5[,colnames(b5)[grepl("2017|2018|2019", colnames(b5))]]
b62018 = b6[,colnames(b6)[grepl("2017|2018|2019", colnames(b6))]]
b72018 = b7[,colnames(b7)[grepl("2017|2018|2019", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2018 = (b52018 - b72018) / (b52018 + b72018)
ndmi2018 = (b52018 - b62018) / (b52018 + b62018)
ndvi2018 = (b52018 - b42018) / (b52018 + b42018)

# Median + IQR
nbr2018median = as.numeric(apply(nbr2018, 1, function(x){median(x, na.rm = TRUE)}))
nbr2018iqr = as.numeric(apply(nbr2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2018median = as.numeric(apply(ndmi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2018iqr = as.numeric(apply(ndmi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2018median = as.numeric(apply(ndvi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2018iqr = as.numeric(apply(ndvi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2018 = data.frame(ID, coords,
                     nbr2018median, ndmi2018median, ndvi2018median,
                     nbr2018iqr, ndmi2018iqr, ndvi2018iqr)

# write median and iqr VIs
VIs2018SF = DFtoSF(VIs2018)
st_write(VIs2018SF, "../data/processed/2018/IIASAtrainingVIs.gpkg")
 
# done
# todo now: implement new features in loadData and RF (features = loadNDVIcovars+new)
# but first do the same for IIASAchange and WUR change

linkIIASAchange = "../data/processed/IIASAchangeFiltered.gpkg"
nameBands <- st_layers(linkIIASAchange)

b4 <- st_read(linkIIASAchange, nameBands$name[4])
b5 <- st_read(linkIIASAchange, nameBands$name[5])
b6 <- st_read(linkIIASAchange, nameBands$name[6])
b7 <- st_read(linkIIASAchange, nameBands$name[7])
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

coordsID = b4[,colnames(b4)[grepl("id|x|y", colnames(b4))]]
  
# Convert to numeric
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

# Split data into 2015, 2016, 2017 and 2018

# 2015
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]]
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]
b62015 = b6[,colnames(b6)[grepl("2014|2015|2016", colnames(b6))]]
b72015 = b7[,colnames(b7)[grepl("2014|2015|2016", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2015 = (b52015 - b72015) / (b52015 + b72015)
ndmi2015 = (b52015 - b62015) / (b52015 + b62015)
ndvi2015 = (b52015 - b42015) / (b52015 + b42015)

nbr2015median = as.numeric(apply(nbr2015, 1, function(x){median(x, na.rm = TRUE)}))
nbr2015iqr = as.numeric(apply(nbr2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2015median = as.numeric(apply(ndmi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2015iqr = as.numeric(apply(ndmi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2015median = as.numeric(apply(ndvi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2015iqr = as.numeric(apply(ndvi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(coordsID, 
                     nbr2015median, ndmi2015median, ndvi2015median,
                     nbr2015iqr, ndmi2015iqr, ndvi2015iqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015, validation = TRUE, coords = c("centroid_x","centroid_y"))
st_write(VIs2015SF, "../data/processed/2015/IIASAchangeVIs.gpkg")

# 2016
b42016 = b4[,colnames(b4)[grepl("2015|2016|2017", colnames(b4))]]
b52016 = b5[,colnames(b5)[grepl("2015|2016|2017", colnames(b5))]]
b62016 = b6[,colnames(b6)[grepl("2015|2016|2017", colnames(b6))]]
b72016 = b7[,colnames(b7)[grepl("2015|2016|2017", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2016 = (b52016 - b72016) / (b52016 + b72016)
ndmi2016 = (b52016 - b62016) / (b52016 + b62016)
ndvi2016 = (b52016 - b42016) / (b52016 + b42016)

# Median + IQR
nbr2016median = as.numeric(apply(nbr2016, 1, function(x){median(x, na.rm = TRUE)}))
nbr2016iqr = as.numeric(apply(nbr2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2016median = as.numeric(apply(ndmi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2016iqr = as.numeric(apply(ndmi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2016median = as.numeric(apply(ndvi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2016iqr = as.numeric(apply(ndvi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2016 = data.frame(coordsID,
                     nbr2016median, ndmi2016median, ndvi2016median,
                     nbr2016iqr, ndmi2016iqr, ndvi2016iqr)
# write median and iqr VIs
VIs2016SF = DFtoSF(VIs2016, validation = TRUE, coords = c("centroid_x","centroid_y"))
st_write(VIs2016SF, "../data/processed/2016/IIASAchangeVIs.gpkg")

# 2017
b42017 = b4[,colnames(b4)[grepl("2016|2017|2018", colnames(b4))]]
b52017 = b5[,colnames(b5)[grepl("2016|2017|2018", colnames(b5))]]
b62017 = b6[,colnames(b6)[grepl("2016|2017|2018", colnames(b6))]]
b72017 = b7[,colnames(b7)[grepl("2016|2017|2018", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2017 = (b52017 - b72017) / (b52017 + b72017)
ndmi2017 = (b52017 - b62017) / (b52017 + b62017)
ndvi2017 = (b52017 - b42017) / (b52017 + b42017)

# Median + IQR
nbr2017median = as.numeric(apply(nbr2017, 1, function(x){median(x, na.rm = TRUE)}))
nbr2017iqr = as.numeric(apply(nbr2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2017median = as.numeric(apply(ndmi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2017iqr = as.numeric(apply(ndmi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2017median = as.numeric(apply(ndvi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2017iqr = as.numeric(apply(ndvi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2017 = data.frame(coordsID,
                     nbr2017median, ndmi2017median, ndvi2017median,
                     nbr2017iqr, ndmi2017iqr, ndvi2017iqr)
# write median and iqr VIs
VIs2017SF = DFtoSF(VIs2017, validation = TRUE, coords = c("centroid_x","centroid_y"))
st_write(VIs2017SF, "../data/processed/2017/IIASAchangeVIs.gpkg")

# 2018
b42018 = b4[,colnames(b4)[grepl("2017|2018|2019", colnames(b4))]]
b52018 = b5[,colnames(b5)[grepl("2017|2018|2019", colnames(b5))]]
b62018 = b6[,colnames(b6)[grepl("2017|2018|2019", colnames(b6))]]
b72018 = b7[,colnames(b7)[grepl("2017|2018|2019", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2018 = (b52018 - b72018) / (b52018 + b72018)
ndmi2018 = (b52018 - b62018) / (b52018 + b62018)
ndvi2018 = (b52018 - b42018) / (b52018 + b42018)

# Median + IQR
nbr2018median = as.numeric(apply(nbr2018, 1, function(x){median(x, na.rm = TRUE)}))
nbr2018iqr = as.numeric(apply(nbr2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2018median = as.numeric(apply(ndmi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2018iqr = as.numeric(apply(ndmi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2018median = as.numeric(apply(ndvi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2018iqr = as.numeric(apply(ndvi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2018 = data.frame(coordsID,
                     nbr2018median, ndmi2018median, ndvi2018median,
                     nbr2018iqr, ndmi2018iqr, ndvi2018iqr)
# write median and iqr VIs
VIs2018SF = DFtoSF(VIs2018, validation = TRUE, coords = c("centroid_x","centroid_y"))
st_write(VIs2018SF, "../data/processed/2018/IIASAchangeVIs.gpkg")

## WUR change
linkWURchange = "../data/processed/WURchangeFiltered.gpkg"
nameBands <- st_layers(linkWURchange)

b4 <- st_read(linkWURchange, nameBands$name[4])
b5 <- st_read(linkWURchange, nameBands$name[5])
b6 <- st_read(linkWURchange, nameBands$name[6])
b7 <- st_read(linkWURchange, nameBands$name[7])
st_geometry(b4)=NULL
st_geometry(b5)=NULL
st_geometry(b6)=NULL
st_geometry(b7)=NULL

coordsID = b4[,colnames(b4)[grepl("id|x|y", colnames(b4))]]

# Convert to numeric
b4 = as.data.frame(sapply(b4[,NewColDates], as.numeric))
b5 = as.data.frame(sapply(b5[,NewColDates], as.numeric))
b6 = as.data.frame(sapply(b6[,NewColDates], as.numeric))
b7 = as.data.frame(sapply(b7[,NewColDates], as.numeric))

# Split data into 2015, 2016, 2017 and 2018

# 2015
b42015 = b4[,colnames(b4)[grepl("2014|2015|2016", colnames(b4))]]
b52015 = b5[,colnames(b5)[grepl("2014|2015|2016", colnames(b5))]]
b62015 = b6[,colnames(b6)[grepl("2014|2015|2016", colnames(b6))]]
b72015 = b7[,colnames(b7)[grepl("2014|2015|2016", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2015 = (b52015 - b72015) / (b52015 + b72015)
ndmi2015 = (b52015 - b62015) / (b52015 + b62015)
ndvi2015 = (b52015 - b42015) / (b52015 + b42015)

nbr2015median = as.numeric(apply(nbr2015, 1, function(x){median(x, na.rm = TRUE)}))
nbr2015iqr = as.numeric(apply(nbr2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2015median = as.numeric(apply(ndmi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2015iqr = as.numeric(apply(ndmi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2015median = as.numeric(apply(ndvi2015, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2015iqr = as.numeric(apply(ndvi2015, 1, function(x){IQR(x, na.rm = TRUE)}))

# save median and iqr in one table
VIs2015 = data.frame(coordsID, 
                     nbr2015median, ndmi2015median, ndvi2015median,
                     nbr2015iqr, ndmi2015iqr, ndvi2015iqr)
# write median and iqr VIs
VIs2015SF = DFtoSF(VIs2015, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2015SF, "../data/processed/2015/WURchangeVIs.gpkg")

# 2016
b42016 = b4[,colnames(b4)[grepl("2015|2016|2017", colnames(b4))]]
b52016 = b5[,colnames(b5)[grepl("2015|2016|2017", colnames(b5))]]
b62016 = b6[,colnames(b6)[grepl("2015|2016|2017", colnames(b6))]]
b72016 = b7[,colnames(b7)[grepl("2015|2016|2017", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2016 = (b52016 - b72016) / (b52016 + b72016)
ndmi2016 = (b52016 - b62016) / (b52016 + b62016)
ndvi2016 = (b52016 - b42016) / (b52016 + b42016)

# Median + IQR
nbr2016median = as.numeric(apply(nbr2016, 1, function(x){median(x, na.rm = TRUE)}))
nbr2016iqr = as.numeric(apply(nbr2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2016median = as.numeric(apply(ndmi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2016iqr = as.numeric(apply(ndmi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2016median = as.numeric(apply(ndvi2016, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2016iqr = as.numeric(apply(ndvi2016, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2016 = data.frame(coordsID,
                     nbr2016median, ndmi2016median, ndvi2016median,
                     nbr2016iqr, ndmi2016iqr, ndvi2016iqr)
# write median and iqr VIs
VIs2016SF = DFtoSF(VIs2016, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2016SF, "../data/processed/2016/WURchangeVIs.gpkg")

# 2017
b42017 = b4[,colnames(b4)[grepl("2016|2017|2018", colnames(b4))]]
b52017 = b5[,colnames(b5)[grepl("2016|2017|2018", colnames(b5))]]
b62017 = b6[,colnames(b6)[grepl("2016|2017|2018", colnames(b6))]]
b72017 = b7[,colnames(b7)[grepl("2016|2017|2018", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2017 = (b52017 - b72017) / (b52017 + b72017)
ndmi2017 = (b52017 - b62017) / (b52017 + b62017)
ndvi2017 = (b52017 - b42017) / (b52017 + b42017)

# Median + IQR
nbr2017median = as.numeric(apply(nbr2017, 1, function(x){median(x, na.rm = TRUE)}))
nbr2017iqr = as.numeric(apply(nbr2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2017median = as.numeric(apply(ndmi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2017iqr = as.numeric(apply(ndmi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2017median = as.numeric(apply(ndvi2017, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2017iqr = as.numeric(apply(ndvi2017, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2017 = data.frame(coordsID,
                     nbr2017median, ndmi2017median, ndvi2017median,
                     nbr2017iqr, ndmi2017iqr, ndvi2017iqr)
# write median and iqr VIs
VIs2017SF = DFtoSF(VIs2017, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2017SF, "../data/processed/2017/WURchangeVIs.gpkg")

# 2018
b42018 = b4[,colnames(b4)[grepl("2017|2018|2019", colnames(b4))]]
b52018 = b5[,colnames(b5)[grepl("2017|2018|2019", colnames(b5))]]
b62018 = b6[,colnames(b6)[grepl("2017|2018|2019", colnames(b6))]]
b72018 = b7[,colnames(b7)[grepl("2017|2018|2019", colnames(b7))]]

# NBR, NDMI, NDVI
nbr2018 = (b52018 - b72018) / (b52018 + b72018)
ndmi2018 = (b52018 - b62018) / (b52018 + b62018)
ndvi2018 = (b52018 - b42018) / (b52018 + b42018)

# Median + IQR
nbr2018median = as.numeric(apply(nbr2018, 1, function(x){median(x, na.rm = TRUE)}))
nbr2018iqr = as.numeric(apply(nbr2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndmi2018median = as.numeric(apply(ndmi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndmi2018iqr = as.numeric(apply(ndmi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

ndvi2018median = as.numeric(apply(ndvi2018, 1, function(x){median(x, na.rm = TRUE)}))
ndvi2018iqr = as.numeric(apply(ndvi2018, 1, function(x){IQR(x, na.rm = TRUE)}))

# Combine in one table
VIs2018 = data.frame(coordsID,
                     nbr2018median, ndmi2018median, ndvi2018median,
                     nbr2018iqr, ndmi2018iqr, ndvi2018iqr)
# write median and iqr VIs
VIs2018SF = DFtoSF(VIs2018, validation = TRUE, coords = c("sample_x","sample_y"))
st_write(VIs2018SF, "../data/processed/2018/WURchangeVIs.gpkg")

# done, all new features created and stored
# TODO: load new features in loadData anad for RF new features
# Already done for loadTrainingData, now also for TrainChange and ValiChange