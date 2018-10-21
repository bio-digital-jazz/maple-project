library(dplyr)

########################## CONNECT TO DB ###################################################
con <- dbConnect(SQLite(), "../../projects/DjangoProjects/data_manager/db.sqlite3")



################################ GET DATA ##################################################
dataElements <- dbGetQuery(con, "SELECT * FROM data_assets_dataelement")
dataElementUses <- dbGetQuery(con, "SELECT * FROM data_assets_dataelementuse" )
acceptedValues <- dbGetQuery(con, "SELECT * FROM data_assets_dataelementacceptedvalue")
dataElementsAcceptedValues <- dbGetQuery(con, "SELECT * FROM data_assets_dataelement_accepted_values")


################### WRITE CSV FILES ########################################################

write.csv(dataElements, file = "./data/dbData/dataElements.csv" )
write.csv(dataElementUses, file = "./data/dbData/dataElementUses.csv" )
write.csv(acceptedValues, file = "./data/dbData/acceptedValues.csv" )
write.csv(dataElementsAcceptedValues, file = "./data/dbData/dataElementsAcceptedValues.csv" )

############################# DISCONNECT ##################################################
dbDisconnect(con)

