library(dplyr)
library(ggplot2)
library(grid)
library(extrafont)



prepareData = function() {
  loadfonts(device = "postscript", quiet=TRUE)
  chart5 <- read.csv("./bowel_age_gender_LHD.csv")
  chart5$calc_rate <- chart5$calc_rate*100
  chart5$calc_LCI <- chart5$calc_LCI*100
  chart5$calc_UCI <- chart5$calc_UCI*100
  
  for (i in 1:dim(chart5)[1]) {
    if (chart5$AgeGroup[i] != "50-74")
    {  	chart5$Order[i] <- 1
    }
    else
    {  chart5$Order[i] <- 2
    }
  }  
  chart5$xyz <- as.character(paste(chart5$Order,chart5$Gender))  
  chart5$AgeGroup <- as.factor(chart5$AgeGroup)
  chart5$xyz <- as.factor(chart5$xyz)
  
  return(chart5)
}



