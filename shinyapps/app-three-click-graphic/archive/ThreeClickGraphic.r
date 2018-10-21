library(tidyverse)
library(ggplot2)
library(png)

### Functions
NA

### Constants
kColPurple = "#48156BBF"

### Globals
NA

#---------------------------------------------------------------------------------------------------
### Import & Tidy & Transform

## Import


## Tidy and transform


#---------------------------------------------------------------------------------------------------
### Visualize & Model


## Fun plot
# grab some data
Image1 = png::readPNG("//fscap01prd.ad.cancerinstitute.org.au/desktops/60159104/Desktop/Drugs2.png")
Rast1  = grid::rasterGrob(Image1, interpolate = T)
Line = data.frame(Year = 2009:2019,
                  Spend = 6:16 + rnorm(11))
Poly = rbind(c(0,0), Line, c(2020, 100), c(0,100))

# Tab1: Infographic
windows(5,5)
InfoGraphic = ggplot() +
  annotation_custom(Rast1, ymin = 0, ymax = 20, xmin = 2009) +
  coord_cartesian(xlim=c(2009,2018), ylim=c(4,20)) +
  geom_line(data=Line, aes(x=Year, y=Spend), size=10, colour="red") +
  geom_polygon(data=Poly, aes(x=Year, y=Spend), fill="white") + 
  labs(title="Drug prices soaring!", subtitle="Infographic",
       x="Year", y="Annual drug spend ($ million)",
       caption="Source: PBAC etc")
#
InfoGraphic

# Tab2: Formal plot
windows(5,5.5)
FormalPlot = ggplot() + 
  geom_col(data=Line, aes(x=Year, y=Spend)) + 
  coord_cartesian(xlim=c(2008.5,2019), ylim=c(0,20)) +
  scale_x_discrete(limits=2009:2018) +
  labs(title="Drug prices increasing over time", subtitle="Publication quality plot",
       x="Year", y="Annual drug spend ($ million)",
       caption="Source: PBAC etc")
#
FormalPlot 

# Tab 3: Data
Line

# Tab 4: Notes
Notes = "Data Source = XXX /n
This is a summary of annual drug spend for cancer drugs by NSW etc/n
Cancer Institute NSW 2018/n
For further info go to www.cancerinstitute.org.au/somepage"


#---------------------------------------------------------------------------------------------------
### End