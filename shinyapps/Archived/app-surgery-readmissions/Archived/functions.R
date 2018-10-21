library(ggplot2)
library(tidyverse)
library(plotly)



uf.getDataset = function(dataPath) {
  return(read_csv(dataPath))
}


uf.getRawMean = function(rawData) {
  return(mean(rawData$Readmitted, na.rm = TRUE))
}


uf.prepareData = function(rawData, 
                          minAge, 
                          maxAge,
                          minLengthOfStay,
                          maxLengthOfStay,
                          lhdChoice) {
  
  rawRedmittedMean = mean(rawData$Readmitted)
  
  filteredData = filter(rawData, 
                        Age > minAge,
                        Age < maxAge,
                        LengthOfStayTotal > minLengthOfStay,
                        LengthOfStayTotal < maxLengthOfStay)
  
  
  if (lhdChoice != "All" || is.null(lhdChoice)) {
    filteredData = filter(filteredData,
                          ResidentLHD %in% lhdChoice)
  }
 
  
  groupedData = filteredData %>% 
    group_by(FacilityCode) %>%
    summarize(
      HospReadmission = mean(Readmitted, na.rm = TRUE), 
      HospVolume = length(Readmitted),
      HospType = FacilityType[1]
    )
  
  
  
  
  
  return(groupedData)
}




uf.renderVisualization = function(data, meanOfFullDataset) {
  p = ggplot() +
    geom_point(data=data, aes(x=HospVolume, 
                              y=HospReadmission, 
                              size=HospVolume, 
                              colour=HospType), 
               alpha=0.75) + 
    xlim(0, 450) + ylim(0, 0.5) + 
    ggtitle("Surgical rates") + 
    xlab("Total surgical volume") + 
    ylab("Readmission rate") +
    geom_hline(yintercept = meanOfFullDataset, 
               colour = "dodgerblue2", 
               size=1)
  
  ggplotly(p)
}


path = "../../../SharedData/SurgicalOutcomesLung.Fake.csv"

data = uf.getDataset(path)


meanOfFullDataset = uf.getRawMean(data)
head(data)



