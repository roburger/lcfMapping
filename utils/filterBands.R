# MSc Thesis
# 13/12/2021
# Outlier filter function

# 1. remove geometry
# 2. remove other columns, keep values only
# 3. apply filter

filterBands = function(band, filterFunction=smoothLoessPlot, dates=dates){
  

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

listDF = list(b1Landsat, b2Landsat, b3Landsat, b4Landsat, b5Landsat, b6Landsat, b7Landsat)
