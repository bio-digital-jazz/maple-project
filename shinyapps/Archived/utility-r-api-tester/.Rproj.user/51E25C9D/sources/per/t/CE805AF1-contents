
library(httr)
library(jsonlite)

data <- GET("http://localhost:7000/data-api/v1/data/")
content = content(data, 'text')
contentJson <- fromJSON(content, flatten = TRUE)
df <- as.data.frame(contentJson)

df

dim(df)
