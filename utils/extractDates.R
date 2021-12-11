# MSc Thesis
# 29/11/2021
# Read in data

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

writeDates = function(){
  
  string = colnames(b1Landsat)[4:194]
  stringSub = substr(dates, 2, 11)
  stringFinal = gsub("\\.", "-", string)
  DFdates = data.frame(date=as.Date(string))
  write.csv(DFdates, paste0(linkData, "processed/dates.csv"), row.names=FALSE)
  
  return(TRUE)
}

extractDates = function(){
  
  DateCSV = read.csv(file="C:/Users/robur/Documents/Thesis/code/data/processed/dates.csv")
  dates = as.Date(DateCSV$date)
  
  return(dates)
}