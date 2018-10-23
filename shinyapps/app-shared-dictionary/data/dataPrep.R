library(dplyr)
library(readr)
library(RSQLite)




con <- dbConnect(SQLite(), "../../projects/DjangoProjects/data_manager/db.sqlite3")
dataElementsDF0 <- dbGetQuery(con, "SELECT * FROM data_assets_dataelement")
dataElementUsesDFO <- dbGetQuery(con, "SELECT * FROM data_assets_dataelementuse")
dataElementsAndUsesDF0 <- dbGetQuery(con, "SELECT * FROM data_assets_dataelementuse_data_element")

####
dataElementsDF1 <-dataElementsDF0 %>%
  select(id,
         element_id,
         name,
         name_is_conformed,
         type,
         definition,
         accepted_values,
         under_review,
         category)

colnames(dataElementsDF1)
colnames(dataElementsDF1) <- c("dataElementID",
                               "Element ID",
                               "Name",
                               "Name is conformed",
                               "Type",
                               "Definition",
                               "Accepted values",
                               "Currently under review",
                               "Category")


readr::write_csv(dataElementsDF1, "./data/preparedData/dataElements.csv")

glimpse(dataElementsDF0)
glimpse(dataElementUsesDFO)
glimpse(dataElementsAndUsesDF0)


dataElementsDF2 <- dataElementsDF0 %>%
  select(id,
         element_id,
         name)


elementsAndUseKeys <- right_join(dataElementsDF2, dataElementsAndUsesDF0, by=c("id"="dataelement_id"))
elementsAndUsesDF1 <- left_join(dataElementUsesDFO, elementsAndUseKeys, by = c("id" = "dataelementuse_id"))

elementsAndUsesDF2 <- elementsAndUsesDF1 %>%
  select(element_use_id,
         risk_level,
         null_value_accepted,
         element_transformation_rules,
         destination_data_asset_id,
         source_data_asset_id,
         destination_element_name,
         source_element_name,
         destination_name_is_conformed,
         element_id)

glimpse(elementsAndUsesDF2)
