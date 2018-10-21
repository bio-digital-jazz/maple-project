library(ggplot2)
library(tidyverse)


####################### IMPORT DATA ######################################
rawData =  read_csv("../../../SharedData/SurgicalOutcomesLung.Fake.csv")

getRawMean = function(rawData) {
  return(mean(rawData$Readmitted, na.rm = TRUE))
}


prepareData = function(rawData) {

  rawRedmittedMean = mean(rawData$Readmitted)
  print(rawRedmittedMean)
  
  groupedData = rawData %>% 
    group_by(FacilityCode) %>%
      summarize(
       HospReadmission = mean(Readmitted, na.rm = TRUE), 
       HospVolume = length(Readmitted),
        HospType = FacilityType[1]
    )
  
  return(groupedData)
}




renderVisualization = function(data) {
  p = ggplot() +
    geom_point(data=data, aes(x=HospVolume, 
                              y=HospReadmission, 
                              size=HospVolume, 
                              colour=HospType), 
                              alpha=0.75) + 
    xlim(0, 450) + ylim(0, 0.5) + 
    ggtitle("Funnel plot to Shiny") + 
    xlab("Total surgical volume") + 
    ylab("Readmission rate") +
    geom_hline(yintercept = getRawMean(rawData), 
               colour = "dodgerblue2", 
               size=1)
  
  
  p
}



d = prepareData(rawData)
renderVisualization(d)




