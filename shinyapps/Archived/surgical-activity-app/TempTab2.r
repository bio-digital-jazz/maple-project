## Tab 2 - Annual volume: this is a stacked area plot

#
#library(NA)

# 1 build local dataframe
Tab2Data = SurgicalActivityAppDF %>%
  filter(Cancer == CancerNameUser,
         FacilityName == FacilityNameUser, 
         ProcedureYear >= YearStartUser, 
         ProcedureYear <= YearEndUser)


## End