# Load libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(ggplot2)

#Build URLS
base_url <- "https://www.econdb.com/api/series/"
cpi_url <- "CPIUS"
ppi_url <- "PPIUS"

full_url_cpi <- base::paste0(base_url,cpi_url)
full_url_ppi <- base::paste0(base_url,ppi_url)

#Call URLs

api_call_cpi <- httr::GET(full_url_cpi)
api_call_ppi <- httr::GET(full_url_ppi)

#Create Dataframes
api_cpi_char <- base::rawToChar(api_call_cpi$content)
api_ppi_char <- base::rawToChar(api_call_ppi$content)

api_cpi_JSON <- jsonlite::fromJSON(api_cpi_char, flatten = TRUE)
api_ppi_JSON <- jsonlite::fromJSON(api_ppi_char, flatten = TRUE)

api_cpi_tbl <- api_cpi_JSON %>%
  map_dfr( ~ .x %>% as_tibble()) %>%
  select(.,-c(value,status,"1:area_code","3:item_code","1220:base_code","GEO:None"))
api_ppi_tbl <- api_ppi_JSON %>%
  map_dfr( ~ .x %>% as_tibble())


api_cpi_cleaned_tbl <- api_cpi_tbl[-(1:6),] %>% mutate(Date= as.Date(dates))
api_ppi_cleaned_tbl <- api_ppi_tbl[-(1:6),] %>% mutate(Date= as.Date(dates))


#Plot
  ggplot(api_cpi_cleaned_tbl, aes(x = Date, y = values, color = "darkred",lty = 'Consumer Price Index')) + 
   geom_line() + 
  scale_x_date(date_labels = "%Y-%m")
  
  ggplot(api_ppi_cleaned_tbl, aes(x = Date, y = values, color="steelblue",lty = 'Produer Price Index')) + 
    geom_line() + 
    scale_x_date(date_labels = "%Y-%m")
 
  

