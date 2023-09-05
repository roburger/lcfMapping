# MSc Thesis
# 15/03/2021
# Explore change points only

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

# Load functions
source("utils/loadData.R")
source("RFfunctionNew.R")
source("utils/subpixelConfusionMatrix.R")
library(ggplot2)
library(data.table)

classes = loadClassNames()

# Load reference data
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




## Find change Only samples

# remove rows that don't experience change, only keep pixels that experience change.
# so basically removing the rows where all classes have zero values.

# 2015-2016
change.actual = change2016[,classes] - change2015[,classes]
sum(rowSums(change.actual==0) !=7)
changeOnly1516 = change.actual[rowSums(change.actual==0) != 7,]
ind_list1516 = rownames(changeOnly1516)
head(ind_list1516,5)

# 2016-2017
change.actual = change2017[,classes] - change2016[,classes]
sum(rowSums(change.actual==0) !=7)
changeOnly1617 = change.actual[rowSums(change.actual==0) != 7,]
ind_list1617 = rownames(changeOnly1617)
head(ind_list1617,5)

# 2017-2018
change.actual = change2018[,classes] - change2017[,classes]
sum(rowSums(change.actual==0) !=7)
changeOnly1718 = change.actual[rowSums(change.actual==0) != 7,]
ind_list1718 = rownames(changeOnly1718)
head(ind_list1718,5)

# keep samples that contain the indices
test = change.actual[rownames(change.actual) %in% c(ind_list1516,ind_list1617,ind_list1718),]
nrow(test)

ind_list = c(ind_list1516,ind_list1617,ind_list1718)

subset_change2015 = change2015[rownames(change.actual) %in% ind_list,]
nrow(subset_change2015)
subset_change2016 = change2016[rownames(change.actual) %in% ind_list,]
nrow(subset_change2016)
subset_change2017 = change2017[rownames(change.actual) %in% ind_list,]
nrow(subset_change2017)
subset_change2018 = change2018[rownames(change.actual) %in% ind_list,]
nrow(subset_change2018)


# Visualization tests with Histogram of LC change
# Tests on change data from 2015-2016 with the grassland class, excluding 0% change
subset_change2015
hist(subset_change2015[,classes]$tree)
hist(abs(changeOnly1516)[,classes]$tree, breaks = c(0,1,99,100))

hist(abs(changeOnly1516$grassland[changeOnly1516$grassland != 0]), freq = TRUE)
hist(abs(changeOnly1516$grassland[changeOnly1516$grassland != 0]), breaks = c(0,1,10,20,40,60,80,99,100), freq = TRUE)

changeOnly1516$tree[changeOnly1516$tree != 0]
change_range = abs(changeOnly1516$grassland[changeOnly1516$grassland != 0])
#change_range
summary(change_range)



hist(beaver1$temp,
     col = "green",
     border = "black",
     prob = TRUE,
     xlab = "temp",
     main = "GFG",
     # Add fill color option
     fill = "lightblue",
     # Add line type option
     lty = "dashed"      
)

lines(density(beaver1$temp),
      lwd = 2,
      col = "chocolate3"
)



## old below


# Filter change points
changeOnly2015 = change2015[!change2015$collection == "original",]
changeOnly2016 = change2016[!change2016$collection == "original",]
changeOnly2017 = change2017[!change2017$collection == "original",]
changeOnly2018 = change2018[!change2018$collection == "original",]

# Save as csv
write.csv(changeOnly2015, "../data/processed/changePoints/ref2015.csv", row.names = F)
write.csv(changeOnly2016, "../data/processed/changePoints/ref2016.csv", row.names = F)
write.csv(changeOnly2017, "../data/processed/changePoints/ref2017.csv", row.names = F)
write.csv(changeOnly2018, "../data/processed/changePoints/ref2018.csv", row.names = F)


## Filter change points for all predicted datasets
# Basic RF:
basic2015 = read.csv("../data/output/wurChange/predictions-2015-median.csv")
basic2016 = read.csv("../data/output/wurChange/predictions-2016-median.csv")
basic2017 = read.csv("../data/output/wurChange/predictions-2017-median.csv")
basic2018 = read.csv("../data/output/wurChange/predictions-2018-median.csv")

basic2015 = cbind("location_id" = change2015$location_id, basic2015)
basic2016 = cbind("location_id" = change2016$location_id, basic2016)
basic2017 = cbind("location_id" = change2017$location_id, basic2017)
basic2018 = cbind("location_id" = change2018$location_id, basic2018)

basic2015 = subset(basic2015, location_id %in% changeOnly2015$location_id)
basic2016 = subset(basic2016, location_id %in% changeOnly2016$location_id)
basic2017 = subset(basic2017, location_id %in% changeOnly2017$location_id)
basic2018 = subset(basic2018, location_id %in% changeOnly2018$location_id)

# Recurrent RF:
rec2015 = read.csv("../data/output/wurChange/predictions-2015-median-recurrent.csv")
rec2016 = read.csv("../data/output/wurChange/predictions-2016-median-recurrent.csv")
rec2017 = read.csv("../data/output/wurChange/predictions-2017-median-recurrent.csv")
rec2018 = read.csv("../data/output/wurChange/predictions-2018-median-recurrent.csv")

rec2015 = cbind("location_id" = change2015$location_id, rec2015)
rec2016 = cbind("location_id" = change2016$location_id, rec2016)
rec2017 = cbind("location_id" = change2017$location_id, rec2017)
rec2018 = cbind("location_id" = change2018$location_id, rec2018)

rec2015 = subset(rec2015, location_id %in% changeOnly2015$location_id)
rec2016 = subset(rec2016, location_id %in% changeOnly2016$location_id)
rec2017 = subset(rec2017, location_id %in% changeOnly2017$location_id)
rec2018 = subset(rec2018, location_id %in% changeOnly2018$location_id)

# Markov:
markov2015 = read.csv("../data/output/markov/smooth2015-basic.csv")
markov2016 = read.csv("../data/output/markov/smooth2016-basic.csv")
markov2017 = read.csv("../data/output/markov/smooth2017-basic.csv")
markov2018 = read.csv("../data/output/markov/smooth2018-basic.csv")

markov2015 = cbind("location_id" = change2015$location_id, markov2015)
markov2016 = cbind("location_id" = change2016$location_id, markov2016)
markov2017 = cbind("location_id" = change2017$location_id, markov2017)
markov2018 = cbind("location_id" = change2018$location_id, markov2018)

markov2015 = subset(markov2015, location_id %in% changeOnly2015$location_id)
markov2016 = subset(markov2016, location_id %in% changeOnly2016$location_id)
markov2017 = subset(markov2017, location_id %in% changeOnly2017$location_id)
markov2018 = subset(markov2018, location_id %in% changeOnly2018$location_id)


# Save as csv
# write.csv(basic2015, "../data/processed/changePoints/basic2015.csv", row.names = F)
# write.csv(basic2016, "../data/processed/changePoints/basic2016.csv", row.names = F)
# write.csv(basic2017, "../data/processed/changePoints/basic2017.csv", row.names = F)
# write.csv(basic2018, "../data/processed/changePoints/basic2018.csv", row.names = F)
# 
# write.csv(rec2015, "../data/processed/changePoints/rec2015.csv", row.names = F)
# write.csv(rec2016, "../data/processed/changePoints/rec2016.csv", row.names = F)
# write.csv(rec2017, "../data/processed/changePoints/rec2017.csv", row.names = F)
# write.csv(rec2018, "../data/processed/changePoints/rec2018.csv", row.names = F)
# 
# write.csv(markov2015, "../data/processed/changePoints/markov2015.csv", row.names = F)
# write.csv(markov2016, "../data/processed/changePoints/markov2016.csv", row.names = F)
# write.csv(markov2017, "../data/processed/changePoints/markov2017.csv", row.names = F)
# write.csv(markov2018, "../data/processed/changePoints/markov2018.csv", row.names = F)


## Statistics
# RMSE, MAE
getStats(basic2015[,classes], changeOnly2015[,classes])$overall
getStats(basic2016[,classes], changeOnly2016[,classes])$overall
getStats(basic2017[,classes], changeOnly2017[,classes])$overall
getStats(basic2018[,classes], changeOnly2018[,classes])$overall

getStats(rec2015[,classes], changeOnly2015[,classes])$overall
getStats(rec2016[,classes], changeOnly2016[,classes])$overall
getStats(rec2017[,classes], changeOnly2017[,classes])$overall
getStats(rec2018[,classes], changeOnly2018[,classes])$overall

getStats(markov2015[,classes], changeOnly2015[,classes])$overall
getStats(markov2016[,classes], changeOnly2016[,classes])$overall
getStats(markov2017[,classes], changeOnly2017[,classes])$overall
getStats(markov2018[,classes], changeOnly2018[,classes])$overall

# SCM -> OA
# Iteratively change datasets below to note down results
s_k = markov2018[,classes]
s_k[rowSums(s_k) == 0,] = rep(100/length(classes),length(classes))
truth = changeOnly2018[,classes]

SCM(s_k/100, truth/100, plot = FALSE, totals = TRUE)

textsize=2.25 #was 1.9

# Per-class accuracies
BasicPlot = ggplotBar(list("2015"=basic2015[,classes],
                           "2016"=basic2016[,classes],
                           "2017"=basic2017[,classes],
                           "2018"=basic2018[,classes]),
                      list(changeOnly2015[,classes],
                           changeOnly2016[,classes],
                           changeOnly2017[,classes],
                           changeOnly2018[,classes]),
                      digits = 1, textsize = textsize, textvjust=-0.5, ylim=55) + 
  theme(legend.position="none") + ylab("a)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

RecPlot = ggplotBar(list("2015"=rec2015[,classes],
                         "2016"=rec2016[,classes],
                         "2017"=rec2017[,classes],
                         "2018"=rec2018[,classes]),
                    list(changeOnly2015[,loadClassNames()],
                         changeOnly2016[,loadClassNames()],
                         changeOnly2017[,loadClassNames()],
                         changeOnly2018[,loadClassNames()]),
                    digits = 1, textsize = textsize, textvjust=-0.5, ylim = 55) + 
  theme(legend.position="none")+ ylab("b)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))
MarkovPlot = ggplotBar(list("2015"=markov2015[,classes],
                            "2016"=markov2016[,classes],
                            "2017"=markov2017[,classes],
                            "2018"=markov2018[,classes]),
                       list(changeOnly2015[,loadClassNames()],
                            changeOnly2016[,loadClassNames()],
                            changeOnly2017[,loadClassNames()],
                            changeOnly2018[,loadClassNames()]),
                       digits = 1, textsize = textsize, textvjust=-0.5, ylim=55)+ 
  theme(legend.position="bottom")+ ylab("c)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

pdf("../../Overig/Visualizations/barplots-perclass/rmse-all-changeOnly-test.pdf", width=1272/175, height=(634*2.2)/175) #height 634*2
gridExtra::grid.arrange(BasicPlot, RecPlot, MarkovPlot, heights=c(40, 40, 50))#nrow=3
dev.off()


## Change year-to-year

# 2015-2016
change.actual = change2016[,classes] - change2015[,classes]
change.basic = basic2016[,classes] - basic2015[,classes]
change.rec = rec2016[,classes] - rec2015[,classes]
change.markov = markov2016[,classes] - markov2015[,classes]

# remove rows that don't experience change, only keep pixels that experience change.
# so basically removing the rows where all classes have zero values. 
sum(rowSums(change.actual==0) !=7)
changeOnly1516 = change.actual[rowSums(change.actual==0) != 7,]



# visualizing histogram of bins
hist(change.actual$tree, col = 'skyblue3', breaks = 10)
hist(
  change.actual$tree,
  col = 'skyblue3',
  breaks = c(-100,-99,-80,-60,-40,-20,-1,0,1,20,40,60,80,100))




# old below ####

getStats(change.basic, change.actual)$overall
getStats(change.rec, change.actual)$overall
getStats(change.markov, change.actual)$overall

# 2016-2017
change.actual = changeOnly2017[,classes] - changeOnly2016[,classes]
change.basic = basic2017[,classes] - basic2016[,classes]
change.rec = rec2017[,classes] - rec2016[,classes]
change.markov = markov2017[,classes] - markov2016[,classes]

getStats(change.basic, change.actual)$overall
getStats(change.rec, change.actual)$overall
getStats(change.markov, change.actual)$overall

# 2017-2018
change.actual = changeOnly2018[,classes] - changeOnly2017[,classes]
change.basic = basic2018[,classes] - basic2017[,classes]
change.rec = rec2018[,classes] - rec2017[,classes]
change.markov = markov2018[,classes] - markov2017[,classes]

getStats(change.basic, change.actual)$overall
getStats(change.rec, change.actual)$overall
getStats(change.markov, change.actual)$overall


# Test actual change
a2015 = changeOnly2015[,classes]
a2016 = changeOnly2016[,classes]
a2017 = changeOnly2017[,classes]
a2018 = changeOnly2018[,classes]

a1516 = a2016-a2015
a1617 = a2017-a2016
a1718 = a2018-a2017

sum(apply(a1516, 1, function(x) any(x > 0)))
sum(apply(a1617, 1, function(x) any(x > 0)))
sum(apply(a1718, 1, function(x) any(x > 0)))

a1516change = a1516[which(apply(a1516, 1, function(x) any(x > 0))),]
a1617change = a1617[which(apply(a1617, 1, function(x) any(x > 0))),]
a1718change = a1718[which(apply(a1718, 1, function(x) any(x > 0))),]

apply(a1516, 2, function(x) sum(x > 0))
apply(a1617, 2, function(x) sum(x > 0))
apply(a1718, 2, function(x) sum(x > 0))





# Save individual per-class plots
pdf("../../Overig/Visualizations/barplots-perclass/rmse-basic-changeOnly.pdf", width=1272/175, height=634/175)
ggplotBar(list("2015"=basic2015[,classes],
               "2016"=basic2016[,classes],
               "2017"=basic2017[,classes],
               "2018"=basic2018[,classes]),
          list(changeOnly2015[,classes],
               changeOnly2016[,classes],
               changeOnly2017[,classes],
               changeOnly2018[,classes]),
          digits = 1, textsize = 1.9, textvjust=-0.5, ylim=55) + 
  theme(legend.position="none") + ylab("a)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))
dev.off()

pdf("../../Overig/Visualizations/barplots-perclass/rmse-markov-changeOnly.pdf", width=1272/175, height=634/175)
ggplotBar(list("2015"=markov2015[,classes],
               "2016"=markov2016[,classes],
               "2017"=markov2017[,classes],
               "2018"=markov2018[,classes]),
          list(changeOnly2015[,loadClassNames()],
               changeOnly2016[,loadClassNames()],
               changeOnly2017[,loadClassNames()],
               changeOnly2018[,loadClassNames()]),
          digits = 1, textsize = 1.9, textvjust=-0.5, ylim=55)+ 
  theme(legend.position="none")+ ylab("c)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))
dev.off()
