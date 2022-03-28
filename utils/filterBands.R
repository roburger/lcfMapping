# MSc Thesis
# 13/12/2021
# Outlier filter function

# 1. remove geometry
# 2. remove other columns, keep values only
# 3. apply filter

filterBands = function(band, filterFunction=smoothLoessPlot, dates=dates){
  
    NewColDates = paste0("X", gsub("-", ".", dates))
  
    #st_geometry(band) <- NULL
    #tempDF = band[,ColDates]
    tempDF = band[,NewColDates]
    
    tempFilter = pbapply(tempDF, 1, filterFunction, dates=dates, res_type="omit", span=0.2,
                         threshold=c(-2,2), threshstat="sd", family="symmetric")
    tempFilter = t(tempFilter)
    #assign(paste0(substr(deparse(substitute(band)),1,2),"Filtered"), tempFilter)
    #rm(tempFilter)
  #}
    tempMatrix = as.matrix(tempDF)
    tempMatrix[is.na(tempFilter)] = NA
    bandFiltered = tempDF
    bandFiltered[,NewColDates] = tempMatrix
  
  return(bandFiltered)
}


# Apply filtered band on all bands
# or in other words: apply new NA's on other bands
# band: refering to the band that needs to be filtered
# filteredBand: refering to the band that has already been filtered

applyFilter <- function(band, filteredBand){
  
  filteredBandMatrix = as.matrix(filteredBand[,NewColDates])
  
  temp = as.matrix(band)[,NewColDates]
  temp[is.na(filteredBandMatrix)] = NA
  
  band[,NewColDates] = temp
  
  return(band)
}



##

# Temporal filter: smooth loess plot (derived from probav)
smoothLoessPlot = function (tsx, QC_good = NULL, dates = NULL, threshold = c(-50, Inf),
                            res_type = c("distance", "sd_distance", "all", "filled","omit", "QC"),
                            span=0.3, family="gaussian", threshstat="none", plot=TRUE, ...) 
{
  if (is.null(QC_good)) {
    QC_good <- as.numeric(!is.na(tsx))
  }
  else {
    QC_good <- as.numeric(QC_good)
  }
  x <- as.numeric(tsx)
  x[QC_good == 0] <- NA
  if (all(is.na(x)))
  {
    warning("Input is all NA")
    return(x)
  }
  if (plot)
    plot(x, type="o", ...)
  if (is.null(dates)) {
    dates <- index(tsx)
  }
  dates <- as.numeric(dates)
  loe <- try(loess(formula = x ~ dates, na.action = "na.omit", span=span, family=family))
  if (class(loe) == "try-error")
    return(x)
  loe_pred <- predict(loe, dates)
  if (plot)
    lines(loe_pred, col="green")
  distance <- (loe_pred - x)
  predmae = mean(abs(distance), na.rm=TRUE)
  predrmse = sqrt(mean(distance^2, na.rm=TRUE))
  xsd = sd(x, na.rm=TRUE)
  xmad = mad(x, na.rm=TRUE)
  if (plot)
    title(sub=paste("MAE:", round(predmae), "RMSE:", round(predrmse), "sd:", round(xsd), "mad:", round(xmad)))
  threshstat = switch(threshstat, none=1, sd=xsd, mad=xmad, mae=predmae, rmse=predrmse)
  threshold = threshold * threshstat
  if (!is.null(threshold)) {
    QC_good[distance < threshold[1] & !is.na(distance)] <- 2
    QC_good[distance > threshold[2] & !is.na(distance)] <- 2
  }
  if (class(tsx) == "zoo") {
    tsx <- zoo(cbind(x = as.numeric(tsx), QC_good, filled = loe_pred), 
               index(tsx))
    return(tsx)
  }
  else {
    x_omit <- x
    x_omit[QC_good != 1] <- NA
    if (plot)
      points(x_omit, type="o", col="red")
    res <- switch(res_type, all = data.frame(x = as.numeric(tsx), 
                                             QC_good = QC_good, filled = loe_pred, distance = round(distance)), 
                  filled = loe_pred, omit = x_omit, QC = QC_good, distance = distance, 
                  sd_distance = (distance/sd(x, na.rm = T)))
    return(res)
  }
}