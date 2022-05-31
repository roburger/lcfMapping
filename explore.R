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

coocc2015 = read.csv("../data/output/markov/smooth2015-coocc.csv")
coocc2016 = read.csv("../data/output/markov/smooth2016-coocc.csv")
coocc2017 = read.csv("../data/output/markov/smooth2017-coocc.csv")
coocc2018 = read.csv("../data/output/markov/smooth2018-coocc.csv")

cooccW2015 = read.csv("../data/output/markov/smooth2015-coocc-100.csv")
cooccW2016 = read.csv("../data/output/markov/smooth2016-coocc-100.csv")
cooccW2017 = read.csv("../data/output/markov/smooth2017-coocc-100.csv")
cooccW2018 = read.csv("../data/output/markov/smooth2018-coocc-100.csv")


iiasa2015 = read.csv("../data/output/markov/smooth2015-coocc-iiasatrain.csv")
iiasa2016 = read.csv("../data/output/markov/smooth2016-coocc-iiasatrain.csv")
iiasa2017 = read.csv("../data/output/markov/smooth2017-coocc-iiasatrain.csv")
iiasa2018 = read.csv("../data/output/markov/smooth2018-coocc-iiasatrain.csv")

iiasaW2015 = read.csv("../data/output/markov/smooth2015-coocc-iiasatrain-100.csv")
iiasaW2016 = read.csv("../data/output/markov/smooth2016-coocc-iiasatrain-100.csv")
iiasaW2017 = read.csv("../data/output/markov/smooth2017-coocc-iiasatrain-100.csv")
iiasaW2018 = read.csv("../data/output/markov/smooth2018-coocc-iiasatrain-100.csv")

wurW2015 = read.csv("../data/output/markov/smooth2015-coocc-wurchange15-100.csv")
wurW2016 = read.csv("../data/output/markov/smooth2016-coocc-wurchange15-100.csv")
wurW2017 = read.csv("../data/output/markov/smooth2017-coocc-wurchange15-100.csv")
wurW2018 = read.csv("../data/output/markov/smooth2018-coocc-wurchange15-100.csv")

cooccNew2015 = read.csv("../data/output/markov/smooth2015-coocc-new-001.csv")
cooccNew2016 = read.csv("../data/output/markov/smooth2016-coocc-new-001.csv")
cooccNew2017 = read.csv("../data/output/markov/smooth2017-coocc-new-001.csv")
cooccNew2018 = read.csv("../data/output/markov/smooth2018-coocc-new-001.csv")

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

getStats(markov2015, change2015[,classes])$overall
getStats(markov2016, change2016[,classes])$overall
getStats(markov2017, change2017[,classes])$overall
getStats(markov2018, change2018[,classes])$overall

getStats(coocc2015, change2015[,classes])$overall
getStats(coocc2016, change2016[,classes])$overall
getStats(coocc2017, change2017[,classes])$overall
getStats(coocc2018, change2018[,classes])$overall

getStats(cooccW2015, change2015[,classes])$overall
getStats(cooccW2016, change2016[,classes])$overall
getStats(cooccW2017, change2017[,classes])$overall
getStats(cooccW2018, change2018[,classes])$overall

getStats(iiasa2015, change2015[,classes])$overall
getStats(iiasa2016, change2016[,classes])$overall
getStats(iiasa2017, change2017[,classes])$overall
getStats(iiasa2018, change2018[,classes])$overall

getStats(iiasaW2015, change2015[,classes])$overall
getStats(iiasaW2016, change2016[,classes])$overall
getStats(iiasaW2017, change2017[,classes])$overall
getStats(iiasaW2018, change2018[,classes])$overall

getStats(wurW2015, change2015[,classes])$overall
getStats(wurW2016, change2016[,classes])$overall
getStats(wurW2017, change2017[,classes])$overall
getStats(wurW2018, change2018[,classes])$overall

getStats(cooccNew2015, change2015[,classes])$overall
getStats(cooccNew2016, change2016[,classes])$overall
getStats(cooccNew2017, change2017[,classes])$overall
getStats(cooccNew2018, change2018[,classes])$overall

# SCM -> OA
# Iteratively change datasets below to note down results
s_k = cooccNew2018[,classes]
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
truth = change2018[,classes]

SCM(s_k/100, truth/100, plot = FALSE, totals = TRUE)$P_overall_accuracy




# 15-16
change.actual = change2016[1:1000,classes] - change2015[1:1000,classes]
change.basic = basic2016[1:1000,] - basic2015[1:1000,]
change.rec = rec2016[1:1000,] - rec2015[1:1000,]
change.markov = markov2016[1:1000,classes] - markov2015[1:1000,classes]
change.coocc = coocc2016[1:1000,classes] - coocc2015[1:1000,classes]
change.cooccNone = cooccNone2016[1:1000,classes] - cooccNone2015[1:1000,classes]

# rmse and mae per class...
getStats(change.markov, change.actual)
getStats(change.coocc, change.actual)
getStats(change.cooccNone, change.actual)
