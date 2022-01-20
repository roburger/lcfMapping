# MSc Thesis
# 19/01/2021
# Load validation data

# Access libraries
library(sf)

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Get Dates
source("utils/extractDates.R")
dates = extractDates()
NewColDates = paste0("X", gsub("-", ".", dates))

# Load in train and vali data (csv)
# ...

# So apparantly training and validation csv's have different land cover classes
# Need to be the same before applying RF on both train and vali....
# Check how to match classes in github code
# utils -> covariate names -> getcommonclasnames()
