
# Transforming the metadata into a dataframe
transform_metadata_to_df <- function(metadata) {
  metadata[[1]] %>% 
    map_dfr(as_tibble) %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_) %>% 
             as_datetime(tz = "UTC")) %>% 
    unnest_wider(location) %>%
    unnest_wider(latLon)
}



# Load lubridate for date-time manipulations
library(lubridate)

# Converting to ISO8601 format
to_iso8601 <- function(datetime, offset) {
  datetime <- with_tz(datetime, "UTC")  # Ensure datetime is in UTC for safety
  offset <- as.integer(offset)  # Convert offset to integer to avoid error etc, lots of errors originally
  
  iso8601(datetime + days(offset)) %>% 
    as.character() %>% 
    paste0("Z")
}

# Transforming the traffic volume data to a dataframe
transform_volumes <- function(data) {
  data$trafficData$volume$byHour$edges %>% 
    map_dfr(~ {
      tibble(
        from = as_datetime(.x$node$from),
        to = as_datetime(.x$node$to),
        volume = sum(.x$node$total$volumeNumbers$volume)  # Ensure volume is fetched right
      )
    })
}
