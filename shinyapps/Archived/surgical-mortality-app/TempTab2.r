# data set to drive the Shiny
#SurgicalOutcomesAppDF2 <- readRDS(file = "SurgicalOutcomesAppDF2.Rda")
PathFile = "H:/Restricted Share/Metadata Repository Project/Projects/SurgicalMortalityApp/SurgicalOutcomesAppDF2.Rda"
SurgicalOutcomesAppDF2 <- readRDS(file = PathFile)

# user choices from Shiny drop downs and sliders etc
FacilityLHDNameUser = "South Western Sydney"
FacilityTypeUser = "Public"
FacilityNameUser = "Concord Hospital" ### can only be 1 hospital
CancerNameUser = "Bladder"
IndicatorNameUser = "LoS01"

# 1 build local dataframe based on user selection
Tab2FacilityDF = SurgicalOutcomesAppDF2 %>%
  filter(CancerName == CancerNameUser,
         FacilityName == FacilityNameUser,
         IndicatorName == IndicatorNameUser) %>%
  na.omit()

# compute YMax
YMax = 1

# Plot the result(s)
SurgicalOutcomesGG2 = ggplot(data = Tab2FacilityDF) +
  coord_cartesian(xlim=c(0,40), ylim=c(0,YMax)) +
  geom_line(aes(x = EWMAX2, y = EWMA2SigmaLow)) +
  geom_line(aes(x = EWMAX2, y = EWMA2SigmaHigh)) +
  geom_line(aes(x = EWMAX2, y = EWMAY)) +
  geom_point(aes(x = EWMAX2, y = EWMAY)) + 
  labs (x = "Quarter 2008-17", y="Proportion")
#
windows(5,5)
SurgicalOutcomesGG2

## End