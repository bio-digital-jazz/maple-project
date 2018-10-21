### Libraries
library(tidyverse)


### Constants - colours from RBCO chart
kColPurple = "#48156BBF"
kBarCols = c(
  rgb( 74/256,  37/256, 116/256),
  rgb(168/256, 143/256, 190/256),
  rgb(128/256,  39/256, 122/256),
  rgb(206/256, 178/256, 205/256),
  rgb(  9/256, 168/256, 224/256),
  rgb(  0/256,  86/256, 161/256),
  rgb(134/256, 151/256, 204/256))

#---------------------------------------------------------------------------------------------------
### Import & Tidy & Transform

# pull in necessary Rda file
RadioTXDF0 = readRDS(file = "RadiotherapyDistanceApp.Rda")

# derive a summary table of radio facilities
RadioFacilities = RadioTXDF0 %>%
  dplyr::group_by(TXFacilityName, TXFacilityLong, TXFacilityLat) %>%
  dplyr::summarise() %>%
  na.omit()

# pull in polygons to draw LHDs
LHDPolygons = readr::read_csv("LHDMapCoords.csv")

#---------------------------------------------------------------------------------------------------
## User choices
CancerCodeUser = "C61"  ### from the menu
LHDNameUser = "Nepean Blue Mountains" ### from the menu

# build cancer specific DF
RadioTXDF1 = RadioTXDF0 %>%
  filter(Cancer==CancerCodeUser)

# build cancer specific and LHD specifi DF
RadioTXDF2 = RadioTXDF1 %>%
  filter(ResidentLHDName==LHDNameUser)

# compute table of counts 
RadioTXDF3 = with(RadioTXDF1, table(ResidentLHDName, DistanceExcess1Cat))

# compute table of proportions
RadioTXDF4 = t(prop.table(RadioTXDF3, 1))

# create LHD specific polygon for map
LHDXPolygon = LHDPolygons %>%
  filter(LHDName==LHDNameUser)

# derive plot coords based on LHD and journeys to set the plot region for the map
XMax = max(LHDXPolygon$Long, RadioTXDF2$ResidentLong, RadioTXDF2$TXFacilityLong, na.rm=TRUE)
XMin = min(LHDXPolygon$Long, RadioTXDF2$ResidentLong, RadioTXDF2$TXFacilityLong, na.rm=TRUE)
XRange = XMax - XMin
YMax = max(LHDXPolygon$Lat, RadioTXDF2$ResidentLat, RadioTXDF2$TXFacilityLat, na.rm=TRUE)
YMin = min(LHDXPolygon$Lat, RadioTXDF2$ResidentLat, RadioTXDF2$TXFacilityLat, na.rm=TRUE)
YRange = YMax - YMin

if (XRange / 1.3 > YRange) {
  YRange = XRange / 1.3
} else {
  XRange = YRange * 1.3
}
XMax = XMin + XRange
YMax = YMin + YRange


# --------------------------------------------------
## Tab 1 stacked bar chart 

### !!! in base R needs converting to ggplot !!!
windows(10,10)
par(mar=c(3,12,2,4))
barplot(t(RadioTXDF3), horiz=TRUE, las=2, bor="white", col=kBarCols[1:6], axes=FALSE, cex.names=0.8)
axis(1, col="white")
mtext(paste0("Excess travel: ", CancerCodeUser), side=3, adj=0)


# --------------------------------------------------
## Tab 2 stacked proportion bar chart

### !!! in base R needs converting to ggplot !!!
windows(10,10)
par(mar=c(3,12,2,4))
barplot(RadioTXDF4, horiz=TRUE, las=2, bor="white", col=kBarCols[1:6], axes=FALSE, cex.names=0.8, xlim=c(0,1.1))
axis(1, 0:5/5, paste0(0:5*20, "%"), col="white")
text(1, 0:15*1.2 + 0.7, paste0("N=", rowSums(RadioTXDF3)), cex=0.8, pos=4)
mtext(paste0("Excess travel: ", CancerCodeUser), side=3, adj=0)


# --------------------------------------------------
## Tab 3 maps of journeys by excess category

# build ggplot object
RadioTXGG3 = ggplot() + 
  coord_cartesian(xlim=c(XMin,XMax), ylim=c(YMin, YMax)) +
  geom_polygon(data=LHDPolygons, aes(x=Long, y=Lat, group=LHDSegment),
               colour = NA, fill="lightgrey", size=0) +
  geom_polygon(data=LHDXPolygon, aes(x=Long, y=Lat, group=LHDSegment),
               colour = NA, fill="darkgrey", size=0) +
  geom_segment(data = RadioTXDF2,
               aes(x = ResidentLong, y = ResidentLat, xend = TXFacilityLong, yend = TXFacilityLat, colour=DistanceExcess1Cat),
               alpha=0.5, size=1.25) + 
  geom_polygon(data=LHDXPolygon, aes(x=Long, y=Lat, group=LHDSegment),
               colour = "white", fill=NA, size=0.5) +
  geom_point(data=RadioFacilities, 
             aes(x=TXFacilityLong, y=TXFacilityLat), colour="red", size=1) +
  ggtitle(paste0(LHDNameUser, ", ", CancerCodeUser)) + 
  labs(colour="Excess travel", caption="CINSW") + 
  facet_grid(. ~ DistanceExcess1Cat) +
  scale_colour_manual(values = kBarCols)
#
NPanels = length(unique(RadioTXDF2$DistanceExcess1Cat))
windows(3.8*NPanels-1, 4)
RadioTXGG3


#---------------------------------------------------------------------------------------------------
### End