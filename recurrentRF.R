# MSc Thesis
# 07/02/2021
# Recurrent RF Tryout

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Access libraries
library(sf)
library(pbapply)
library(ranger)
library(plyr)
source("utils/loadData.R")
source("RFfunction.R")

# Link to data folder
linkData <- "C:/Users/robur/Documents/Thesis/code/data/"

## 

# Create initial formula string
subformula = paste0("~", paste(features, collapse = "+"))
# Adjust formula
formula = paste0("tree", subformula, "+previousyear")

# Run RF
rfmodel = ranger(formula, train, seed = 0xbadcafe, quantreg=quantreg)

set = years[[4]][,features]
set = cbind(years[[4]][,features],medpred2017$tree)
            
test = predict(rfmodel, set, 
                 type=PredictType, quantiles=PredictQuantiles)$predictions

pred2015 = read.csv("../data/output/wurChange/predictions-2015-ndvi-median.csv")
pred2016 = read.csv("../data/output/wurChange/predictions-2016-ndvi-median.csv")
pred2017 = read.csv("../data/output/wurChange/predictions-2017-ndvi-median.csv")
pred2018 = read.csv("../data/output/wurChange/predictions-2018-ndvi-median.csv")

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

### 
# Try new function for recurrent RF
# still should be applied to change dataset as vali TODO
# 
# runRecurrentRF <- function(train=loadTrainingData(), years=loadValidationData(), 
#                             features=loadNDVIcovarsNames(), PredictType="response"){
#   
#   # Set median voting settings
#   quantreg = ifelse(PredictType=="response", FALSE, TRUE)
#   PredictQuantiles = 0.5
#   
#   # Load class names
#   classes = loadClassNames()
#   
#   # Create initial formula string
#   subformula = paste0("~", paste(features, collapse = "+"))
#   
#   # Create empty df's to store predictions of every year
#   for (i in 1:length(years)){
#     #print(paste0(i, ": create empty DF"))
#     temp = data.frame(matrix(ncol=length(classes), nrow=nrow(years[[i]])))
#     colnames(temp)=classes
#     assign(paste0("predictions", names(years)[i]), temp)
#   }
#   
#   n = 1
#   # Apply RF for each class
#   for (class in classes){
#     print(paste0("Class (", n, "/", length(classes), "): ", class))
#     n = n + 1
#     
#     # Adjust formula
#     formula = paste0(class, subformula)
#     
#     # Run RF
#     rfmodel = ranger(formula, train, seed = 0xbadcafe, quantreg=quantreg)
#     
#     # Make predictions for every year (2015, 2016, 2017, 2018)
#     for (j in 1:length(years)){
#       
#       print(paste0("Predicting for ", names(years[j]), "..."))
#       
#       # Apply RF-model for predictions
#       output = predict(rfmodel, years[[j]][,features], type=PredictType, quantiles=PredictQuantiles)$predictions
#       
#       # Store class prediction of every year in df
#       temp = eval(parse(text = paste0("predictions", names(years)[j])))
#       temp[,class] = output
#       assign(paste0("predictions", names(years)[j]), temp)
#     }
#     #if (class=="shrub"){break}
#   }
#   
#   # Scale predictions
#   # TODO: loop over predictions DF's of all years and scale
#   #predictions = predictions / rowSums(predictions, na.rm=T) * 100
#   print("Scaling the predictions...")
#   for (i in 1:length(years)){
#     #assign(paste0("predictions", names(years)[i]), temp)
#     temp = eval(parse(text = paste0("predictions", names(years)[i])))
#     temp = temp / rowSums(temp, na.rm=T) * 100
#     temp = as.matrix(temp)
#     temp[is.nan(temp)] = 0
#     assign(paste0("predictions", names(years)[i]), as.data.frame(temp))
#   }
#   
#   # Return all yearly predicted DF's 
#   return(list(predictions2015, predictions2016, predictions2017, predictions2018))
# }


# Try to train on prev yr only
# check with RMSE and MAE
copy = dataVali2016
names(copy)
names(copy[,classes])
names(pred2015)
copy = cbind(copy, previous=pred2015$tree)
#copy[,classes] = pred2015

formula = "tree~previous"
recurrentRFmodel = ranger(formula, copy, seed = 0xbadcafe, quantreg=quantreg)
output = predict(recurrentRFmodel, copy["previous"],
                 type=PredictType, quantiles=PredictQuantiles)$predictions
# not good

## THIS RECURRENT MODEL BELOW SEEMS TO WORK !
# TODO: MAKE A FUNCTION OF THIS, AND PUT IN RF FUNCTIOJN
# try to rbind train + prev, to make recurrent training set for 2016
train2016 = dataTrain
prev = data.frame("tree"=change2015$tree,"pred_tree"=pred2015$tree)
prev = data.frame(change2015[,c("tree",features)], "pred_tree"=pred2015$tree)

test = rbind.fill(train2016, prev)

# Replace NAs with mean
CM = mean(test$pred_tree, na.rm = T)
test$pred_tree[is.na(test$pred_tree)] = CM

formula=paste0(formula, "+pred_tree")

recurrentRFmodel = ranger(formula, test, seed = 0xbadcafe, quantreg=quantreg)
vali = data.frame(years[[j]][,features], eval(parse(text=paste0("pred",names(years)[j-1]))))
vali = data.frame(years[[j]][,features], 
                  "pred_tree"= eval(parse(text=paste0("pred",names(years)[j-1])))[,"tree"])

output = predict(recurrentRFmodel, vali, 
                 type=PredictType, quantiles=PredictQuantiles)$predictions
round(sqrt(mean(unlist(output - change2018[,"tree"])^2)), digits = 1)
round(mean(abs(unlist(output  - change2018[,"tree"]))), digits = 1)

## 
# Try recurrent RF
# Copy of RF function belo tweaked to being recurrent

# I guess that it should be trained for each year
# so train in the for loop within for loop.

# now running this not on wurChange as vali, but wur2015 as vali
# just to check rmse and mae (should be better),
# and compare with Dainius's rmse and mae of median forest
listRecDFs = runRecurrentRF(train=dataTrain, years=years, PredictType="quantiles")
rec2015_vali = listRecDFs[[1]]
rec2016_vali = listRecDFs[[2]]
rec2017_vali = listRecDFs[[3]]
rec2018_vali = listRecDFs[[4]]

rmse2015_vali = round(sqrt(mean(unlist(rec2015_vali - dataVali2015[,classes])^2)), digits = 1)
rmse2016_vali = round(sqrt(mean(unlist(rec2016_vali - dataVali2016[,classes])^2)), digits = 1)
rmse2017_vali = round(sqrt(mean(unlist(rec2017_vali - dataVali2017[,classes])^2)), digits = 1)
rmse2018_vali = round(sqrt(mean(unlist(rec2018_vali - dataVali2018[,classes])^2)), digits = 1)
mae2015_vali = round(mean(abs(unlist(rec2015_vali  - dataVali2015[,classes]))), digits = 1)
mae2016_vali = round(mean(abs(unlist(rec2016_vali  - dataVali2016[,classes]))), digits = 1)
mae2017_vali = round(mean(abs(unlist(rec2017_vali  - dataVali2017[,classes]))), digits = 1)
mae2018_vali = round(mean(abs(unlist(rec2018_vali  - dataVali2018[,classes]))), digits = 1)

write.csv(rec2015, "../data/output/wurChange/predictions-2015-ndvi-median-recurrent.csv", row.names = F)
write.csv(rec2016, "../data/output/wurChange/predictions-2016-ndvi-median-recurrent.csv", row.names = F)
write.csv(rec2017, "../data/output/wurChange/predictions-2017-ndvi-median-recurrent.csv", row.names = F)
write.csv(rec2018, "../data/output/wurChange/predictions-2018-ndvi-median-recurrent.csv", row.names = F)

# Voor morgen: 
# nog een error in het runnen van runRecurrentRF.
# maar reccureent model seems to work

runRecurrentRF <- function(train=loadTrainingData(), years=loadValidationData(), 
                            features=loadNDVIcovarsNames(), PredictType="response"){
  
  # Set median voting settings
  quantreg = ifelse(PredictType=="response", FALSE, TRUE)
  PredictQuantiles = 0.5
  
  # Load class names
  classes = loadClassNames()
  
  # Create initial formula string
  subformula = paste0("~", paste(features, collapse = "+"))
  
  # Create empty df's to store predictions of every year
  for (i in 1:length(years)){
    #print(paste0(i, ": create empty DF"))
    temp = data.frame(matrix(ncol=length(classes), nrow=nrow(years[[i]])))
    colnames(temp)=classes
    assign(paste0("rec", names(years)[i]), temp)
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
        prevDF = data.frame(years[[1]][,c(class,features)], "prev"=rec2015[[class]])
        #print(paste0("rec2015: ",class(rec2015[[class]])))
        #print(paste0("prevDF: ", class(prevDF)))
        #print(paste0("prevDF$prev: ", class(prevDF$prev)))
        
        train.recurrent = rbind.fill(train, prevDF)
        
        # Replace NAs with mean
        #print("replacing NAs for the mean")
        
        CM = mean(train.recurrent[["prev"]], na.rm = T)
        #print(CM)
        #column = train.recurrent$prev
        #column[is.na(column)] = CM
        #train.recurrent$prev = column
        train.recurrent$prev[is.na(train.recurrent$prev)] = CM
        
        #print("seems to work")
        
        # Adjust formula to include previous year's output
        recFormula = paste0(formula, "+prev")
        
        # Run recurrent RF
        print("Building recurrent model for 2016, 2017, 2018...")
        rfmodel = ranger(recFormula, train.recurrent, seed = 0xbadcafe, quantreg=quantreg)
        
      }
      
      if (j != 1){
        # Include previous year's output in validation set 
        vali = data.frame(years[[j]][,features], 
                          "prev"= eval(parse(text=paste0("rec",names(years)[j-1])))[[class]])
      }
      else {
        vali = data.frame(years[[j]][,features])
      }
      
      # Apply RF-model for predictions
      print(paste0("Predicting for ", names(years[j]), "..."))
      output = predict(rfmodel, vali,
                       type=PredictType, quantiles=PredictQuantiles)$predictions
      
      # Store class prediction of every year in df
      temp = eval(parse(text = paste0("rec", names(years)[j])))
      temp[,class] = as.numeric(output)
      assign(paste0("rec", names(years)[j]), temp)
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
