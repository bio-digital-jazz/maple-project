## Tab 1 - Surgical outcomes


#
library(tidyverse)
library(ggplot2)

# helper function
fMakeFunnelPolygons = function(Mean, XMax, YMin=0, YMax=1) {

  #
  if(Mean > 1) Mean = 1
  if(Mean < 0) Mean = 0
  
  # Conpute the funnels based on beta distribution
  XMax = XMax * 1.0
  P = round(Mean, 2)
  Q = 1 - P
  N = 1:XMax
  E = N * P
  
  Y2SDLow  = qbeta(0.023, E, N-E+1)
  Y2SDHigh = qbeta(0.977, E+1, N-E)
  
  Y3SDLow  = qbeta(0.0013, E, N-E+1)
  Y3SDHigh = qbeta(0.9987, E+1, N-E)
  
  # clip the polygons here based on the 2x YLims
  Y2SDLow[Y2SDLow < YMin] = YMin
  Y2SDHigh[Y2SDHigh > YMax] = YMax
  Y2SD = c(Y2SDHigh, rev(Y2SDLow))
  
  Y3SDLow[Y3SDLow < YMin] = YMin
  Y3SDHigh[Y3SDHigh > YMax] = YMax
  Y3SD = c(Y3SDHigh, rev(Y3SDLow))
  
  X = c(N, rev(N))
  
  data.frame(X=X, Y2SD = Y2SD, Y3SD = Y3SD)
}


# data set to drive the Shiny
SurgicalOutcomesAppDF <- readRDS(file = "SurgicalOutcomesAppDF.Rda")

# user choices from Shiny drop downs and sliders etc
FacilityLHDNameUser = "South Western Sydney"
FacilityTypeUser = "Public"
FacilityNameUser = "Nepean Hospital" ### can be more than 1 hospital
CancerNameUser = "Lung"
IndicatorNameUser = "Surv1Year"

# 1 build local dataframe based on user selection
Tab1FacilityDF = SurgicalOutcomesAppDF %>%
  filter(CancerName == CancerNameUser,
         FacilityName == FacilityNameUser,
         IndicatorName == IndicatorNameUser)

# get NSW mean based on user selection
Tab1NSWDF = SurgicalOutcomesAppDF %>%
  filter(CancerName == CancerNameUser,
         FacilityName == "NSW",
         FacilityType == FacilityTypeUser,
         IndicatorName == IndicatorNameUser)
NSWMean = Tab1NSWDF$FacilityMeanRaw

# get info on other facilities in state
Tab1FacilityOtherDF = SurgicalOutcomesAppDF %>%
  filter(CancerName == CancerNameUser,
         FacilityName != "NSW",
         FacilityType == FacilityTypeUser,
         IndicatorName == IndicatorNameUser)

# derive maximum x-axis value
XMax = max(100, Tab1FacilityOtherDF$FacilityN * 1.1, na.rm=TRUE)

# Plot the result(s)
SurgicalOutcomesGG = ggplot(data = Tab1FacilityDF) +
  coord_cartesian(xlim=c(0,XMax), ylim=c(0,1)) +
  geom_polygon(data = fMakeFunnelPolygons(NSWMean, XMax, 0, 1), aes(x = X, y = Y2SD), fill = "#BEBEBE70", colour = NA) +
  geom_polygon(data = fMakeFunnelPolygons(NSWMean, XMax, 0, 1), aes(x = X, y = Y3SD), fill = "#BEBEBE70", colour = NA) +
  geom_hline(yintercept = NSWMean, colour="dodgerblue2") +
  geom_point(aes(x=FacilityN, y=FacilityMeanMarg), size=2) + 
  geom_rug(data=Tab1FacilityOtherDF, aes(FacilityN), colour="darkgrey") +
  labs (x = "Total volume 2014-17", y="Proportion")
#
windows(5,5)
SurgicalOutcomesGG


## End