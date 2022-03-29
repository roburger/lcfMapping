# MSc Thesis
# 28/01/2021
# Random Forest function

runRandomForest <- function(train=loadTrainingData(), years=loadValidationData(), 
                            features=loadFeaturesNames(), PredictType="response"){
  
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
    assign(paste0("predictions", names(years)[i]), temp)
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
      
      print(paste0("Predicting for ", names(years[j]), "..."))
      
      # Apply RF-model for predictions
      output = predict(rfmodel, years[[j]][,features], type=PredictType, quantiles=PredictQuantiles)$predictions
      
      # Store class prediction of every year in df
      temp = eval(parse(text = paste0("predictions", names(years)[j])))
      temp[,class] = output
      assign(paste0("predictions", names(years)[j]), temp)
    }
    #if (class=="shrub"){break}
  }
  
  # Scale predictions
  # TODO: loop over predictions DF's of all years and scale
  #predictions = predictions / rowSums(predictions, na.rm=T) * 100
  print("Scaling the predictions...")
  for (i in 1:length(years)){
    #assign(paste0("predictions", names(years)[i]), temp)
    temp = eval(parse(text = paste0("predictions", names(years)[i])))
    temp = temp / rowSums(temp, na.rm=T) * 100
    temp = as.matrix(temp)
    temp[is.nan(temp)] = 0
    assign(paste0("predictions", names(years)[i]), as.data.frame(temp))
  }
  
  # Return all yearly predicted DF's 
  return(list(predictions2015, predictions2016, predictions2017, predictions2018))
}




# original
# runRandomForest <- function(train=loadTrainingData(), vali=loadValidationData(), 
#                             features=loadNDVIcovarsNames()){
#   
#   # Load class names
#   classes = loadClassNames()
#   
#   # Create initial formula string
#   subformula = paste0("~", paste(features, collapse = "+"))
#   
#   # Create empty df for predictions
#   predictions = data.frame(matrix(ncol=length(classes), nrow=nrow(vali)))
#   colnames(predictions)=classes
#   
#   # Apply RF for each class
#   for (class in classes){
#     print(class)
#     
#     # Adjust formula
#     formula = paste0(class, subformula)
#     
#     # Run RF
#     rfmodel = ranger(formula, train)
#     
#     # Apply RF-model for predictions
#     output = predict(rfmodel, vali[,features])$predictions
#     
#     # Store per class prediction in df
#     predictions[,class]=output
#   }
#   
#   # Scale predictions
#   predictions = predictions / rowSums(predictions, na.rm=T) * 100
#   
# }

getStats <- function(predictions, reference=loadValidationData(), classes=loadClassNames()){
  
  # Create empty df 
  statistics = data.frame(matrix(ncol=length(classes), nrow=2))
  colnames(statistics)=classes
  rownames(statistics)=c("RMSE","MAE")
  
  for (class in classes){
    # RMSE
    RMSE = sqrt(mean((predictions[[class]]-reference[[class]])^2))
    statistics["RMSE",class] = RMSE
    # MAE
    MAE = mean(abs(predictions[[class]]-reference[[class]]))
    statistics["MAE",class] = MAE
  }
  
  # Add overall RMSE and MAE as column
  rmse = round(sqrt(mean(unlist(predictions - reference)^2)), digits = 1)
  mae = round(mean(abs(unlist(predictions  - reference))), digits = 1)
  statistics$overall = c(rmse, mae)
  
  # Round stats to 1 digit -> more interpretable
  statsRounded = round(statistics, digits = 1)
  
  return(statsRounded)
}


##  Recurrent RF-model
# For each land cover class:
# 1. build basic RF-model, trained on IIASA 2015 dataset
# 2. creates predictions for 2015, with basic RF
# 3. build recurrent RF-model, trained on 2016 features and 2015 predictions
# 4. creates predictions for 2016, 2017 and 2018

runRecurrentRF <- function(train=loadTrainingData(),years=loadValidationData(), 
                           features=loadFeaturesNames(), PredictType="response"){
  
  # Set median voting settings
  quantreg = ifelse(PredictType=="response", FALSE, TRUE)
  PredictQuantiles = 0.5
  
  # Load class names
  classes = loadClassNames()
  
  # Create initial formula string
  subformula = paste0("~", paste(features, collapse = "+"))
  
  # Create empty df's to store yearly predictions on validation set of change
  for (i in 1:length(years)){
    temp = data.frame(matrix(ncol=length(classes), nrow=nrow(years[[i]])))
    colnames(temp)=classes
    assign(paste0("rec", names(years)[i]), temp)
  }
  
  # Create empty df's to store yearly predictions on training locations
  for (i in 1:length(years)){
    temp = data.frame(matrix(ncol=length(classes), nrow=nrow(dataTrain)))
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
      # Not retrained every year
      # Train recurrent model for 2016, based on features of 2016 and 2015 class output 
      if (j == 2){
        
        # train.recurrent = train + prev
        train.recurrent = data.frame(train[,c(class,features)], 
                                     "prev" = predTrain2015[[class]]) # maybe CBIND?
        
        # Adjust formula to include previous year's output
        recFormula = paste0(formula, "+prev")
        
        # Train recurrent RF model...
        print("Building recurrent model for 2016, 2017, 2018...")
        rfmodel = ranger(recFormula, train.recurrent, seed = 0xbadcafe, quantreg=quantreg)
      }
      
      # Set up validation datasets for basic and recurrent RF-models
      if (j != 1){
        # Include previous year's prediction in validation set
        temp = eval(parse(text = paste0("rec", names(years)[j-1])))
        vali = data.frame(years[[j]][,features], temp[class])
        names(vali)[length(names(vali))] = "prev"
        
        # Also predict on training locations
        trainX = data.frame(dataTrain[,features],
                            "prev" = eval(parse(text=paste0("predTrain",names(years)[j-1])))[[class]])
      }
      
      # Set-up validation data for the year 2015
      else {
        vali = data.frame(years[[j]][,features])
        trainX = train[,features]
      }
      
      # Apply RF-model to predict on validation set (either basic or recurrent)
      print(paste0("Predicting for ", names(years[j]), "..."))
      output = predict(rfmodel, vali,
                       type=PredictType, quantiles=PredictQuantiles)$predictions
      
      # Store class prediction of every year in df
      temp = eval(parse(text = paste0("rec", names(years)[j])))
      temp[,class] = as.numeric(output)
      assign(paste0("rec", names(years)[j]), temp)
      
      # Also predict on training locations (input for recurrent model)
      print(paste0("Predicting on training locations for ", names(years[j]), "..."))
      output = predict(rfmodel, trainX, type=PredictType, 
                       quantiles=PredictQuantiles)$predictions
      
      # Store class prediction in df
      temp = eval(parse(text = paste0("predTrain", names(years)[j])))
      temp[,class] = as.numeric(output)
      assign(paste0("predTrain", names(years)[j]), temp)
      
    }
    #if (class=="shrub"){break} # testing the model
  }
  
  # Scale predictions
  # TODO: loop over predictions DF's of all years and scale
  #predictions = predictions / rowSums(predictions, na.rm=T) * 100
  print("Scaling the predictions...")
  for (i in 1:length(years)){
    temp = eval(parse(text = paste0("rec", names(years)[i])))
    temp = temp / rowSums(temp, na.rm=T) * 100
    temp = as.matrix(temp)
    temp[is.nan(temp)] = 0
    assign(paste0("rec", names(years)[i]), as.data.frame(temp))
  }
  
  # Return all yearly predicted DF's 
  return(list(rec2015, rec2016, rec2017, rec2018))
}