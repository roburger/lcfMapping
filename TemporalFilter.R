# MSc Thesis
# 29/11/2021
# Temporal Filter

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Import packages
library(sf)

source("utils/filterBands.R")
b1Test
lapply(as.matrix(b1Test[1:100,]), filterBands)
lapply(X = b1Landsat, FUN = filterBands)

b1Filtered <- filterBands(b1Landsat, smoothLoessPlot, dates)
mean(is.na(b1Filtered))
mean(is.na(b1Values[1:100,]))

b2Filtered <- filterBands(b2Landsat, smoothLoessPlot, dates)
#ColDates niet hetzelfde
mean(is.na(b2Filtered))
mean(is.na(b2Test[1:100,]))


# Get Dates
source("utils/extractDates.R")
dates = extractDates()
dates
ColDates = paste0("X", gsub("-", ".", dates), "_SR_B1")
NewColDates = paste0("X", gsub("-", ".", dates))
colnames(b1Test[,ColDates]) = NewColDates

## Test Temporal Filtering and Parameters ##
TestTS = sample(1:nrow(b1Landsat), 50)

st_geometry(b1Landsat) <- NULL

b1Test = as.matrix(as.data.frame(b1Landsat[,4:194]))

for (i in TestTS)
{smoothLoessPlot(b1Test[i,], dates=dates, res_type="omit", span=0.2, 
                 threshold=c(-2,2), main=i, threshstat="sd", family="symmetric")}

## Apply Temporal Filtering ##
startTime = Sys.time()
b1MaskTest = pbapply(b1Test[1:10000,], 1, smoothLoessPlot, dates=dates, res_type="omit", span=0.2,
        threshold=c(-2,2), threshstat="sd", family="symmetric")
endTime = Sys.time()
time1000 = endTime - startTime
time1000
b1MaskTest = t(b1MaskTest)


b1Mask = pbapply(b1Test, 1, smoothLoessPlot, dates=dates, res_type="omit", span=0.2,
                     threshold=c(-2,2), threshstat="sd", family="symmetric")
b1Mask = t(b1Mask)


## Start again from zero ##
b1Test = b1Landsat
st_geometry(b1Test) <- NULL

DFcoords = b1Test[c("x","y")]
write.csv(DFcoords, paste0(linkData, "processed/IIASAtrainingCoords.csv"), row.names=F)
coords = c("x","y")
b1Values = b1Test[,ColDates]

tempSF = st_as_sf(b1Test, coords=coords, dim="XY", remove=FALSE, crs=4326)
names(tempSF)[names(tempSF) == "geometry"] = "geom"
st_geometry(tempSF) = "geom"
st_write(tempSF, "C:/Users/robur/Documents/Thesis/code/data/processed/testSF.gpkg")
#Dit boven werkt goed, TODO now: write forloop for filter and writing to gpkg layers.

## Test results ##
mean(is.na(b1Test[1:1000,]))
mean(is.na(b1MaskTest))
mean(colMeans(is.na(b1MaskTest)))
sum(is.na(b1Test[1:10000,]) != is.na(b1MaskTest)) / length(b1MaskTest)
# Test shows 2% changed to NA

## Test results of b1 ##
mean(is.na(b1Test))
mean(is.na(b1Mask))
mean(colMeans(is.na(b1MaskTest)))
sum(is.na(b1Test) != is.na(b1Mask)) / length(b1Mask)
# B1 filter shows 2% increase in NA, from 43% to 45% NA



InputMatrix = b1Landsat[,ColDates]
InputMatrix[is.na(b1Mask)] = NA


## Save output ##
st_write(InputMatrix, "C:/Users/robur/Documents/Thesis/code/data/processed/b1Filtered.csv")
# b1Mask moet naar dataframe met geometry






## Function ##
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
