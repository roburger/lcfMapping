# MSc Thesis
# 15/03/2021
# Explore outcomes

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

source("utils/loadData.R")
#source("RFfunction.R")
source("RFfunctionNew.R")
source("utils/subpixelConfusionMatrix.R")

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

classes = loadClassNames()

basic2015 = read.csv("../data/output/wurChange/predictions-2015-median.csv")
basic2016 = read.csv("../data/output/wurChange/predictions-2016-median.csv")
basic2017 = read.csv("../data/output/wurChange/predictions-2017-median.csv")
basic2018 = read.csv("../data/output/wurChange/predictions-2018-median.csv")

rec2015 = read.csv("../data/output/wurChange/predictions-2015-median-recurrent.csv")
rec2016 = read.csv("../data/output/wurChange/predictions-2016-median-recurrent.csv")
rec2017 = read.csv("../data/output/wurChange/predictions-2017-median-recurrent.csv")
rec2018 = read.csv("../data/output/wurChange/predictions-2018-median-recurrent.csv")

markov2015 = read.csv("../data/output/markov/smooth2015-basic.csv")
markov2016 = read.csv("../data/output/markov/smooth2016-basic.csv")
markov2017 = read.csv("../data/output/markov/smooth2017-basic.csv")
markov2018 = read.csv("../data/output/markov/smooth2018-basic.csv")

#markov2015 = read.csv("../data/output/markov/smooth2015-unscaled.csv")
#markov2016 = read.csv("../data/output/markov/smooth2016-unscaled.csv")
#markov2017 = read.csv("../data/output/markov/smooth2017-unscaled.csv")
#markov2018 = read.csv("../data/output/markov/smooth2018-unscaled.csv")

## Explore outcomes

# 2018 - 2015
diff.actual = change2018[,classes] - change2015[,classes]
diff.pred = rec2016 - rec2015

sum(apply(diff.actual,1,function(x) all(x==0))) # rows having change (27.202)
sum(apply(diff.actual,1,function(x) any(!(x==0)))) # rows having no-change (3287)

changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]

changeRows2018 = rownames(changeRows)

# 2017 - 2015
diff.actual = change2017[,classes] - change2015[,classes]
changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]
changeRows2017 = rownames(changeRows)

# 2016 - 2015
diff.actual = change2016[,classes] - change2015[,classes]
changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]
changeRows2016 = rownames(changeRows)

length(unique(c(changeRows2018,changeRows2017,changeRows2016)))

# 1516, 1517, 1518, 1617, 1618, 1718

# 1617
diff.actual = change2017[,classes] - change2016[,classes]
changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]
changeRows1617 = rownames(changeRows)

# 1618
diff.actual = change2018[,classes] - change2016[,classes]
changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]
changeRows1618 = rownames(changeRows)

# 1718
diff.actual = change2018[,classes] - change2017[,classes]
changeRows = apply(diff.actual,1,function(x) all(x==0))
changeRows = diff.actual[!changeRows,]
changeRows1718 = rownames(changeRows)

length(unique(c(changeRows2018,changeRows2017,changeRows2016,
                changeRows1617,changeRows1618,changeRows1718)))

which(change2016$collection=="change1")

# 2018 - 2015
# Compare predicted change vs actual change
diff.actual = change2018[,classes] - change2015[,classes]
diff.actual.change = apply(diff.actual,1,function(x) {sum(x[x>0])})

diff.pred = basic2018[,classes] - basic2015[,classes]
diff.pred.change = apply(diff.pred,1,function(x) {sum(x[x>0])})

# Recurrent change vs actual change
compare = data.frame(diff.actual.change, diff.pred.change)
diffRMSErec = round(sqrt(mean(unlist(compare$diff.pred.change - compare$diff.actual.change)^2)), 
                    digits = 1)
diffMAErec = round(mean(abs(unlist(compare$diff.pred.change - compare$diff.actual.change))), 
                   digits = 1)

# Basic change vs actual change
diff.basic = basic2018[,classes] - basic2015[,classes]
diff.basic.change = apply(diff.basic,1,function(x) {sum(x[x>0])})
diffRMSEbasic = round(sqrt(mean(unlist(diff.basic.change - diff.actual.change)^2)),  
                      digits = 1)
diffMAEbasic = round(mean(abs(unlist(diff.basic.change - diff.actual.change))), 
                     digits = 1)

# Markov change vs actual change
diff.markov = markov2018[,classes] - markov2015[,classes]
diff.markov.change = apply(diff.markov,1,function(x) {sum(x[x>0])})
diffRMSEmarkov = round(sqrt(mean(unlist(diff.markov.change - diff.actual.change)^2)),  
                      digits = 1)
diffMAEmarkov = round(mean(abs(unlist(diff.markov.change - diff.actual.change))), 
                     digits = 1)

# Change (2018-2015)
#       Basic   Rec   Markov
# RMSE  34.9    24.4  25.2
# MAE   20.1    8.0   14.6

# OA    (calc for each class?)
# not reallyt possible with change values? OA is used for fractions?

# remark 2: now looked a change between 15-18, but should look at year-to-year change?

## 
# year-to-year change
# 15-16, 16-17, 17-18
# RMSE and MAE
# + per class

# 15-16
change.actual = change2016[,classes] - change2015[,classes]
change.basic = basic2016 - basic2015
change.rec = rec2016 - rec2015
change.markov = markov2016[,classes] - markov2015[,classes]

# rmse and mae per class...
change.basic.stats1516 = getStats(change.basic, change.actual)
change.rec.stats1516 = getStats(change.rec, change.actual)
change.markov.stats1516 = getStats(change.markov, change.actual)

change.basic.stats1516
change.rec.stats1516
change.markov.stats1516

# 16-17
change.actual = change2017[,classes] - change2016[,classes]
change.basic = basic2017 - basic2016
change.rec = rec2017 - rec2016
change.markov = markov2017[,classes] - markov2016[,classes]

# rmse and mae per class...
change.basic.stats1617 = getStats(change.basic, change.actual)
change.rec.stats1617 = getStats(change.rec, change.actual)
change.markov.stats1617 = getStats(change.markov, change.actual)

change.basic.stats1617
change.rec.stats1617
change.markov.stats1617

# 17-18
change.actual = change2018[,classes] - change2017[,classes]
change.basic = basic2018 - basic2017
change.rec = rec2018 - rec2017
change.markov = markov2018[,classes] - markov2017[,classes]

# rmse and mae per class...
change.basic.stats1718 = getStats(change.basic, change.actual)
change.rec.stats1718 = getStats(change.rec, change.actual)
change.markov.stats1718 = getStats(change.markov, change.actual)

change.basic.stats1718
change.rec.stats1718
change.markov.stats1718


## Yearly stats (OA, RMSE, MAE)
s_k = basic2015
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
truth = change2015[,classes]

test = SCM(s_k/100, truth/100, plot = FALSE, totals = TRUE)
cat(round(test$P_overall_accuracy,3)*100, "+", round(test$U_overall_accuracy,3)*100)

getStats(basic2015, change2015[,classes])
getStats(basic2016, change2016[,classes])
getStats(basic2017, change2017[,classes])
getStats(basic2018, change2018[,classes])

getStats(rec2015, change2015[,classes])
getStats(rec2016, change2016[,classes])
getStats(rec2017, change2017[,classes])
getStats(rec2018, change2018[,classes])

getStats(markov2015, change2015[,classes])
getStats(markov2016, change2016[,classes])
getStats(markov2017, change2017[,classes])
getStats(markov2018, change2018[,classes])
