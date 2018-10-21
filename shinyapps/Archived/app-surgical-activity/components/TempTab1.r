## Tab 1 - Activity: this is a beeswarm plot

#
library(ggbeeswarm)

SurgicalActivityAppDF <- readRDS(file = "SurgicalActivityAppDF.Rda")

# user choices from Shint sdrop downs and sliders etc
FacilityLHDNameUser = "South Eastern Sydney"
FacilityNameUser = "Prince of Wales Hospital"
CancerNameUser = "Breast"
YearStartUser = 2009
YearEndUser = 2017

# 1 build local dataframe
Tab1Data = SurgicalActivityAppDF %>%
  filter(Cancer == CancerNameUser,
         FacilityName == FacilityNameUser, 
         ProcedureYear >= YearStartUser, 
         ProcedureYear <= YearEndUser)

# compute some summary stats
ResectionCount = nrow(Tab1Data)
ProcedureCount = length(unique(Tab1Data$ProcedureGroup2))
ProcedureRank1 = NA
YearCount = YearEndUser - YearStartUser + 1

# 2 plot it
PlotHeight = 1.5 + ProcedureCount / 1.6
PlotWidth = 1 + 2 * YearCount
windows(PlotWidth, PlotHeight)

ggplot(data=Tab1Data, aes(x=ProcedureGroup2, y=ProcedureDate)) +
  geom_beeswarm(priority='density',cex=1, colour="#0000004D") +
  scale_y_date(date_breaks ='1 years', date_labels = "%b\n%Y") +
  ggtitle(paste0(FacilityNameUser, ": ", CancerNameUser), 
          subtitle = FacilityLHDNameUser) + 
  xlab("") + ylab("") +
  theme(panel.grid.minor = element_blank()) +
  coord_flip()


# 3 Create text from the inference engine
ExplanationTxt = "Each dot represents the index surgical episode"
ObservationsTxt = paste0("Time period = ", YearStartUser, " to ", YearEndUser, "\n",
                         "Number of surgeries = ", ResectionCount, "\n",
                         "Number of procedure types = ", ProcedureCount)
InsightsTxt = " < insight engine under construction > "

## End