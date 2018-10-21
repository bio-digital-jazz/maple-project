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
                          lhdChoice,
                          genderFilter,
                          cancerTypeFilter,
                          procedureFilter) {
  
  rawRedmittedMean = mean(rawData$Readmitted)
  
  filteredData = filter(rawData, 
                        Age > minAge,
                        Age < maxAge,
                        LengthOfStayTotal > minLengthOfStay,
                        LengthOfStayTotal < maxLengthOfStay)
  
  
  print(filteredData)
  
  if (lhdChoice != "All" || is.null(lhdChoice)) {
    filteredData = filter(filteredData,
                          ResidentLHD %in% lhdChoice)
  }
  
  if (genderFilter != "All" || is.null(genderFilter)) {
    filteredData = filter(filteredData,
                          Sex %in% genderFilter)
  }
  
  if (cancerTypeFilter!= "All" || is.null(cancerTypeFilter)) {
    filteredData = filter(filteredData,
                          CancerName %in% cancerTypeFilter)
  }
  
  
  if (procedureFilter!= "All" || is.null(procedureFilter)) {
    filteredData = filter(filteredData,
                          ProcedureGroup %in% procedureFilter)
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



### block out the render function temp
if(1==2) {
uf.renderVisualization = function(data, meanOfFullDataset) {
  p = ggplot() +
    geom_point(data=data, aes(x=HospVolume, 
                              y=HospReadmission, 
                              size=HospVolume, 
                              colour=HospType), 
               alpha=0.75) + 
    xlim(0, 450) + ylim(0, 0.5) + 
    xlab("Total surgical volume") + 
    ylab("Readmission rate") +
    geom_hline(yintercept = meanOfFullDataset, 
               colour = "dodgerblue2", 
               size=1)
  
  ggplotly(p)
}
}

### updated render with funnels
uf.renderVisualization = function(data, meanOfFullDataset) {
  p = ggplot() +
    geom_polygon(data=ufMakeFunnelPoly(0.105, 450, 0, 0.5), aes(x = X, y = Y2SD), fill = "#BEBEBE70", colour = NA) +
    geom_polygon(data=ufMakeFunnelPoly(0.105, 450, 0, 0.5), aes(x = X, y = Y3SD), fill = "#BEBEBE70", colour = NA) +
    geom_point(data=data, aes(x=HospVolume, 
                              y=HospReadmission, 
                              size=HospVolume, 
                              colour=HospType), 
               alpha=0.75) + 
    xlim(0, 450) + ylim(0, 0.5) + 
    xlab("Total surgical volume") + 
    ylab("Readmission rate") +
    geom_hline(yintercept = meanOfFullDataset, 
               colour = "dodgerblue2", 
               size=1)
  
  ggplotly(p)
}


#
ufMakeFunnelPoly = function(Mean, XMax, YMin=0, YMax=1) {
  
  #
  if(Mean > 1) Mean = 1
  if(Mean < 0) Mean = 0
  
  #
  XMax = XMax * 1.0
  P = round(Mean, 2)
  Q = 1 - P
  N = 1:XMax
  E = N * P
  
  Y2SDLow  = qbeta(0.023, E, N-E+1)
  Y2SDHigh = qbeta(0.977, E+1, N-E)
  
  Y3SDLow  = qbeta(0.0013, E, N-E+1)
  Y3SDHigh = qbeta(0.9987, E+1, N-E)
  
  ## need to clip the polygons here based on the 2x YLims
  Y2SDLow[Y2SDLow < YMin] = YMin
  Y2SDHigh[Y2SDHigh > YMax] = YMax
  Y2SD = c(Y2SDHigh, rev(Y2SDLow))
  
  Y3SDLow[Y3SDLow < YMin] = YMin
  Y3SDHigh[Y3SDHigh > YMax] = YMax
  Y3SD = c(Y3SDHigh, rev(Y3SDLow))
  
  X = c(N, rev(N))
  
  data.frame(X=X, Y2SD = Y2SD, Y3SD = Y3SD)
}


path = "./Data/SurgicalOutcomesLung.Fake.csv"

data = uf.getDataset(path)


meanOfFullDataset = uf.getRawMean(data)
head(data)



