library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)
library(glue) 

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
  ) 


# Add a new file in the functions-folder called data_transformations.r.
# This file should contain a function called transform_metadata_to_df.
# The function transform_metadata_to_df should complete the transformation of 
# stations_metadata to a data frame 


transform_metadata_to_df <- function(stations_metadata) {
  stations_metadata[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_, format = "%Y-%m-%d 00:00:00")) %>%
    mutate(latestData = as_datetime(latestData, tz = "Europe/Berlin"))%>%
    force_tz("UTC") %>%
    unnest_wider(location) %>% 
    unnest_wider(latLon)
} 

## Task 4 

## 4a

to_iso8601 <- function(datetime, offset) {
  # Add the offset to the datetime
  adjusted_datetime <- datetime + days(offset)
  
  # Format the adjusted datetime in ISO8601 format with 'Z' to indicate UTC
  iso8601_datetime <- format(anytime(adjusted_datetime), format = "%Y-%m-%dT%H:%M:%SZ")
  
  return(iso8601_datetime)
}

# Example usage
to_iso8601(as_datetime("2016-09-01 10:11:12"), 0)
to_iso8601(as_datetime("2016-09-01 10:11:12"), -4)


## 5 

#transform_volumes <- function(vol_qry){
#  df <- stations_metadata$trafficdata[[1]]
#}

#transform_volumes <- httr::GET("https://www.vegvesen.no/trafikkdata/api/")
#transform_volumes$status_code
#cont <- transform_volumes$content
#char <- rawToChar(transform_volumes$content)
#df <-jsonlite::fromJSON(char)

#transform_volumes$char %>% 
#  map(as_tibble) %>% 
#  list_rbind()

#transform_volumes <- function(stations_metadata){ 
#  df <- stations_metadata$trafficRegistrationPoints[[1]] %>% 
#    map(as_tibble) %>% 
#    list_rbind() %>% 
#    unnest_wider(edges) %>%
#    unnest_wider(node) %>% 
#    mutate(from = map_chr(from, 1, .default = NA_character_), 
#           to = map_chr(to, 1, .default = NA_character_)) %>% 
#    mutate(from = as_datetime(from, tz = "UTC"),
#           to = as_date(to,tz = "UTC")) %>% 
#    mutate(total = map(total,unlist)) %>% 
#    mutate(
#      volume = map_dbl(total, "volumeNumbers.volume")
#    ) %>% 
#    select(-total)

#  return(df)

#}

# fail to transform the json-return from the API to a data frame in both methods above
#I think I am looking for the wrong dokument here to transform as the document is not a valid JSON format?
