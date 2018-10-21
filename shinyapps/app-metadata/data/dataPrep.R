library(RSQLite)
library(dplyr)
library(readr)

con <- dbConnect(SQLite(), "../../projects/DjangoProjects/data_manager/db.sqlite3")


dataAssetsDF0 <- as_tibble(dbGetQuery(con, "SELECT * FROM data_assets_dataasset"))
usersDF0 <- as_tibble(dbGetQuery(con, "SELECT * FROM auth_user"))
dataAssetRolesDF0<- as_tibble(dbGetQuery(con, "SELECT * FROM data_assets_dataassetrole"))

relatedResourcesDF0 <- as_tibble(dbGetQuery(con, "SELECT * FROM related_resources_dataassetrelatedresource"))
dataAssetRelatedResourcesDF0 <- as_tibble(dbGetQuery(con, "SELECT * FROM related_resources_dataassetrelatedresource_data_asset"))



# Create assets

dataAssetsDF1 <- dataAssetsDF0 %>%
  mutate(currently_active = case_when(
    currently_active == 0 ~ "No",
    currently_active == 1 ~ "Yes"
  )) %>%
  mutate(used_in_shared_dictionary = case_when(
    used_in_shared_dictionary == 0 ~ "No",
    used_in_shared_dictionary == 1 ~ "Yes"
  )) %>%
  mutate(governed_as_external_data = case_when(
    governed_as_external_data == 0 ~ "No",
    governed_as_external_data == 1 ~ "Yes"
  )) %>%
  mutate(available_for_analytics = case_when(
    available_for_analytics == 0 ~ "No",
    available_for_analytics == 1 ~ "Yes"
  )) %>%
  mutate(under_review = case_when (
    under_review == 0 ~ "No",
    under_review == 1 ~ "Yes"
  )) %>%
  mutate(utlizes_external_data = case_when(
    utlizes_external_data == 0 ~"No",
    utlizes_external_data == 1 ~ "Yes"
  ))

  



glimpse(dataAssetsDF1)

dataAssetsDF0$name


colnames(dataAssetsDF1) <- c('dataAssetID',
                             'Owning organisation',
                             'Owning business unit',
                             'Name',
                             'Asset ID',
                             'Version',
                             'Emergency shutdown contact',
                             'Emergency shutdown organisation',
                             'Coverage start date',
                             'Coverage end date',
                             'Data limiting marker',
                             'Update frequency',
                             'Currently active',
                             'Used in shared dictionary',
                             'Governed as external data',
                             'Available for analytics',
                             'Usage notes available',
                             'Data location',
                             'Description',
                             'Data location type',
                             'Under review',
                             'Utilizes external data',
                             'Type'
                             )





readr::write_csv(dataAssetsDF1, "./data/preparedData/dataAssets.csv")

# Create users and roles
usersAndRolesDF0 = usersDF0 %>%
  left_join(dataAssetRolesDF0, by = c("id" = "user_id" )) %>%
  select(type, 
         first_name, 
         last_name, 
         start_date, 
         end_date,
         status,
         data_asset_id)



colnames(usersAndRolesDF0) <- c("Type", 
                                "First name",
                                "Last name",
                                "Start date",
                                "End date",
                                "Status",
                                "dataAssetID")

readr::write_csv(usersAndRolesDF0, "./data/preparedData/usersAndRoles.csv")


# Create assets and resources

assetsAndResourcesDF0 =  inner_join(relatedResourcesDF0, 
                                    dataAssetRelatedResourcesDF0,  
                                    by = c("id" = "dataassetrelatedresource_id"))
assetsAndResourcesDF0 %>%
    select(dataasset_id,
           title,
           description,
           date_created,
           next_review_date,
           version,
           hprm_reference) -> assetsAndResourcesDF1

colnames(assetsAndResourcesDF1) = c('dataAssetID',
                                    'Title',
                                    'Description',
                                    'Date created',
                                    'Next review date',
                                    'Version',
                                    'Reference')





readr::write_csv(assetsAndResourcesDF1, "./data/preparedData/assetsAndResources.csv")

# Create Metadata Table


