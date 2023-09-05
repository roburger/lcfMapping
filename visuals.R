# MSc Thesis
# 15/03/2021
# Explore outcomes

# Set working directory
setwd("~/Thesis/code/lcfMapping/")

source("utils/loadData.R")
source("RFfunctionNew.R")
library(ggplot2)
library(data.table)


basic_rmse = data.frame(class=loadClassNames(), 
                        "2015"=c(22.9, 18.2, 36.2, 20.7, 10.9, 25.0, 18.3),
                        "2016"=c(23.1, 18.4, 37.0, 21.2, 11.4, 25.3, 18.4),
                        "2017"=c(23.1, 18.2, 37.3, 21.2, 11.7, 25.5, 18.8),
                        "2018"=c(23.0, 18.1, 36.7, 21.4, 12.5, 25.6, 18.6))

new = transpose(basic_rmse)

ggplot(basic_rmse, aes(x=class, y=X2015)) + 
  geom_bar(stat = "identity")


###

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


markov2015 = read.csv("../data/output/markov/smooth2015-coocc-new-001.csv")
markov2016 = read.csv("../data/output/markov/smooth2016-coocc-new-001.csv")
markov2017 = read.csv("../data/output/markov/smooth2017-coocc-new-001.csv")
markov2018 = read.csv("../data/output/markov/smooth2018-coocc-new-001.csv")


ggplotBar = function(ModelsToPlot, Truth, statistic="RMSE", ylab=NULL, digits=0, textsize=2.5, textvjust=-0.1, ylim=40, ...)
{
  if (is.null(ylab)) ylab=statistic
  ModelStats=NULL
  for (i in 1:length(ModelsToPlot))
  {
    LMM = AccuracyStatTable(ModelsToPlot[[i]], Truth[[i]], ...)[statistic]
    LMM[[paste0(statistic,"R")]] = round(LMM[[statistic]], digits = digits) # For printing rounded numbers
    LMM$Year=names(ModelsToPlot)[i]
    ClassNames = prettyNames(rownames(LMM))
    
    #ClassNames = rownames(LMM)
    #LMM$Class=factor(ClassNames, c("Overall", ClassNames[ClassNames != "Overall"]))
    LMM$Class=factor(ClassNames, c("Overall",ClassNames[ClassNames != "Overall"]))
    #LMM$Class=factor(prettyNames)
    
    ModelStats = rbind(ModelStats, LMM)
  }
  ModelStats$Year = factor(ModelStats$Year, levels = names(ModelsToPlot))
  print(ModelStats)
  #return(ModelStats)
  # ggplot(ModelStats, aes_string("Year", statistic, fill="Class")) +
  ggplot(ModelStats, aes_string("Class", statistic, fill="Year")) +
    geom_col(position = "dodge", colour="black") +
    geom_text(aes_string(label=sprintf("%s", paste0(statistic,"R"))), position=position_dodge(width = 0.9), vjust=textvjust, size=textsize) +
    #scale_fill_manual(name = "Class", values = GetCommonClassColours(TRUE, 0.1)) +
    coord_cartesian(clip = 'off') + ylab(ylab) + theme(axis.title.x=element_blank()) +
    scale_y_continuous(limits = c(0, ylim), breaks = seq(0,50,10))
  
}

ggplotBarNew = function(ModelsToPlot, Truth, statistic="RMSE", ylab=NULL, digits=0, textsize=2.5, textvjust=-0.1, ylim=40, ...)
{
  if (is.null(ylab)) ylab=statistic
  ModelStats=NULL
  for (i in 1:length(ModelsToPlot))
  {
    LMM = AccuracyStatTable(ModelsToPlot[[i]], Truth[[i]], ...)[statistic]
    LMM[[paste0(statistic,"R")]] = round(LMM[[statistic]], digits = digits) # For printing rounded numbers
    LMM$Model=names(ModelsToPlot)[i]
    ClassNames = prettyNames(rownames(LMM))
    
    #ClassNames = rownames(LMM)
    #LMM$Class=factor(ClassNames, c("Overall", ClassNames[ClassNames != "Overall"]))
    LMM$Class=factor(ClassNames, c("Overall",ClassNames[ClassNames != "Overall"]))
    #LMM$Class=factor(prettyNames)
    
    ModelStats = rbind(ModelStats, LMM)
  }
  ModelStats$Model = factor(ModelStats$Model, levels = names(ModelsToPlot))
  print(ModelStats)
  #return(ModelStats)
  # ggplot(ModelStats, aes_string("Class", statistic, fill="Year")) +
  ggplot(ModelStats, aes_string("Class", statistic, fill="Model")) +
    geom_col(position = "dodge", colour="black") +
    geom_text(aes_string(label=sprintf("%s", paste0(statistic,"R"))), position=position_dodge(width = 0.9), vjust=textvjust, size=textsize) +
    #scale_fill_manual(name = "Class", values = GetCommonClassColours(TRUE, 0.1)) +
    coord_cartesian(clip = 'off') + ylab(ylab) + theme(axis.title.x=element_blank()) +
    scale_y_continuous(limits = c(0, ylim), breaks = seq(0,50,10))
  
}


## Make a new Barplot
# Switch the years with the models

plot2015 = ggplotBarNew(list("Basic RF"=basic2015,"Recurrent RF"=rec2015,"Markov"=markov2015),
                      list(change2015[,loadClassNames()],
                           change2015[,loadClassNames()],
                           change2015[,loadClassNames()]),
                      digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="none") + ylab("2015") + theme(axis.title.y = element_text(face="bold",angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

plot2016 = ggplotBarNew(list("Basic RF"=basic2016,"Recurrent RF"=rec2016,"Markov"=markov2016),
                        list(change2016[,loadClassNames()],
                             change2016[,loadClassNames()],
                             change2016[,loadClassNames()]),
                        digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="none") + ylab("2016") + theme(axis.title.y = element_text(face="bold",angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

plot2017 = ggplotBarNew(list("Basic RF"=basic2017,"Recurrent RF"=rec2017,"Markov"=markov2017),
                        list(change2017[,loadClassNames()],
                             change2017[,loadClassNames()],
                             change2017[,loadClassNames()]),
                        digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="none") + ylab("2017") + theme(axis.title.y = element_text(face="bold",angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

plot2018 = ggplotBarNew(list("Basic RF"=basic2018,"Recurrent RF"=rec2018,"Markov"=markov2018),
                        list(change2018[,loadClassNames()],
                             change2018[,loadClassNames()],
                             change2018[,loadClassNames()]),
                        digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="bottom") + ylab("2018") + theme(axis.title.y = element_text(face="bold",angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))


pdf("~/Thesis/Paper/visualizations/barplots-perclass/rmse-models-bold.pdf", width=1272/175, height=(634*2.2)/175) #height 2.2 ipv 2
gridExtra::grid.arrange(plot2015, plot2016, plot2017, plot2018, heights=c(30, 30, 30, 40))#nrow=3
dev.off()




## old below
# textsize was 1.9, now 2.4

#ggplotBar(list("2015"=basic2015,"2016"=basic2016,"2017"=basic2017,"2018"=basic2018),
                      
BasicPlot = ggplotBar(list("2015"=basic2015,"2016"=basic2016,"2017"=basic2017,"2018"=basic2018),
                      list(change2015[,loadClassNames()],
                           change2016[,loadClassNames()],
                           change2017[,loadClassNames()],
                           change2018[,loadClassNames()]),
                      digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="none") + ylab("a)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))
                                                                                                                       
RecPlot = ggplotBar(list("2015"=rec2015,"2016"=rec2016,"2017"=rec2017,"2018"=rec2018),
                    list(change2015[,loadClassNames()],
                         change2016[,loadClassNames()],
                         change2017[,loadClassNames()],
                         change2018[,loadClassNames()]),
                    digits = 1, textsize = 2.25, textvjust=-0.5) + 
  theme(legend.position="none")+ ylab("b)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))
MarkovPlot = ggplotBar(list("2015"=markov2015,"2016"=markov2016,"2017"=markov2017,"2018"=markov2018),
                       list(change2015[,loadClassNames()],
                            change2016[,loadClassNames()],
                            change2017[,loadClassNames()],
                            change2018[,loadClassNames()]),
                       digits = 1, textsize = 2.25, textvjust=-0.5)+ 
  theme(legend.position="bottom")+ ylab("c)") + theme(axis.title.y = element_text(angle=0,vjust=0.5,margin=margin(t=0,r=10,b=0,l=0)))

pdf("../../visualizations/barplots-perclass/rmse-all-new-test.pdf", width=1272/175, height=(634*2.2)/175) #height 2.2 ipv 2
gridExtra::grid.arrange(BasicPlot, RecPlot, MarkovPlot, heights=c(40, 40, 50))#nrow=3
dev.off()







# test below + functions
res2015 = AccuracyStatTable(basic2015,change2015[,loadClassNames()])
res2016 = AccuracyStatTable(basic2016,change2016[,loadClassNames()])
res2017 = AccuracyStatTable(basic2017,change2017[,loadClassNames()])
res2018 = AccuracyStatTable(basic2018,change2018[,loadClassNames()])

pdf("../../Overig/Visualizations/barplots-perclass/rmse-basic.pdf", width=1272/175, height=634/175)
#gridExtra::grid.arrange(RMSEPlot, heights=c(40, 60))
ggplotBar(list("2015"=basic2015,"2016"=basic2016,"2017"=basic2017,"2018"=basic2018),
          list(change2015[,loadClassNames()],
               change2016[,loadClassNames()],
               change2017[,loadClassNames()],
               change2018[,loadClassNames()]),
          digits = 1, textsize = 1.9, textvjust=-0.5)
dev.off()

pdf("../../Overig/Visualizations/barplots-perclass/rmse-rec.pdf", width=1272/175, height=634/175)
#gridExtra::grid.arrange(RMSEPlot, heights=c(40, 60))
ggplotBar(list("2015"=rec2015,"2016"=rec2016,"2017"=rec2017,"2018"=rec2018),
          list(change2015[,loadClassNames()],
               change2016[,loadClassNames()],
               change2017[,loadClassNames()],
               change2018[,loadClassNames()]),
          digits = 1, textsize = 1.9, textvjust=-0.5)
dev.off()

pdf("../../Overig/Visualizations/barplots-perclass/rmse-markov.pdf", width=1272/175, height=634/175)
#gridExtra::grid.arrange(RMSEPlot, heights=c(40, 60))
ggplotBar(list("2015"=markov2015,"2016"=markov2016,"2017"=markov2017,"2018"=markov2018),
          list(change2015[,loadClassNames()],
               change2016[,loadClassNames()],
               change2017[,loadClassNames()],
               change2018[,loadClassNames()]),
          digits = 1, textsize = 1.9, textvjust=-0.5)
dev.off()


#Old
ggplotBar = function(ModelsToPlot, Truth, statistic="RMSE", ylab=NULL, digits=0, textsize=2.5, textvjust=-0.1, ylim=40, ...)
{
  if (is.null(ylab)) ylab=statistic
  ModelStats=NULL
  for (i in 1:length(ModelsToPlot))
  {
    LMM = AccuracyStatTable(ModelsToPlot[[i]], Truth[[i]], ...)[statistic]
    LMM[[paste0(statistic,"R")]] = round(LMM[[statistic]], digits = digits) # For printing rounded numbers
    LMM$Year=names(ModelsToPlot)[i]
    ClassNames = prettyNames(rownames(LMM))
    
    #ClassNames = rownames(LMM)
    #LMM$Class=factor(ClassNames, c("Overall", ClassNames[ClassNames != "Overall"]))
    LMM$Class=factor(ClassNames, c("Overall",ClassNames[ClassNames != "Overall"]))
    #LMM$Class=factor(prettyNames)
    
    ModelStats = rbind(ModelStats, LMM)
  }
  ModelStats$Year = factor(ModelStats$Year, levels = names(ModelsToPlot))
  print(ModelStats)
  #return(ModelStats)
  # ggplot(ModelStats, aes_string("Year", statistic, fill="Class")) +
  ggplot(ModelStats, aes_string("Class", statistic, fill="Year")) +
    geom_col(position = "dodge", colour="black") +
    geom_text(aes_string(label=sprintf("%s", paste0(statistic,"R"))), position=position_dodge(width = 0.9), vjust=textvjust, size=textsize) +
    #scale_fill_manual(name = "Class", values = GetCommonClassColours(TRUE, 0.1)) +
    coord_cartesian(clip = 'off') + ylab(ylab) + theme(axis.title.x=element_blank()) +
    scale_y_continuous(limits = c(0, ylim), breaks = seq(0,50,10))
  
}



AccuracyStats = function(predicted, observed, relative=FALSE)
{
  RMSE = sqrt(mean(unlist(predicted - observed)^2))
  MAE = mean(abs(unlist(predicted - observed)))
  ME = mean(unlist(predicted - observed))
  #RMSEAdj = sqrt(mean(unlist(predicted - observed - ME)^2))
  Result = data.frame(RMSE, MAE, ME)#, RMSEAdj)
  if (relative)
    Result = Result/mean(unlist(observed))
  return(Result)
}

AccuracyStatTable = function(predicted, observed, relative=FALSE)
{
  Result = AccuracyStats(predicted, observed, relative=relative)
  row.names(Result) = "Overall"
  for (i in 1:ncol(observed)){
    RMSE = sqrt(mean(unlist(predicted[,i] - observed[,i])^2))
    MAE = mean(abs(unlist(predicted[,i] - observed[,i])))
    ME = mean(unlist(predicted[,i] - observed[,i]))
    ColResult = data.frame(RMSE, MAE, ME)
    if (relative)
      ColResult = ColResult/mean(unlist(observed[,i]))
    Result = rbind(Result, ColResult)
    #Result = ColResult
    #row.names(Result)[i+1] = names(observed[i])
    row.names(Result)[i+1] = names(observed[i])
  }
  Result = Result[-1,]
  return(Result)
}


prettyNames = function(uglyNames){
  
  NameMap = function(x){
    
    switch(x,
           Overall= "Overall",
           tree = "Trees",
           shrub = "Shrubs",
           grassland = "Grassland",
           crops = "Crops",
           urban_built_up = "Built-Up",
           bare = "Bare",
           water = "Water")
  }
  
  if (is.list(uglyNames))
  {
    Result = lapply(uglyNames, sapply, NameMap)
    names(Result) = sapply(names(Result), NameMap)
  }
  else
    Result = sapply(uglyNames, NameMap)
  
  return(Result)
}
