# Webscraping
# 1.0 --- Libraries --- 
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
# 1.1 --- Collect Product Families of ROSE ---

url_home <- "https://www.rosebikes.de/fahrräder"

#Read in the HTML for the entire homepage
html_home <- read_html(url_home)

#Webscrape the product families from ROSE bikes

bike_family_tbl <- html_home %>%
  
    #Parent node
    html_nodes(".columns") %>%
  
    #Get the nodes for the families
    html_nodes(css = ".catalog-navigation__list-item > a") %>%
    #And extract the information of the title attribute
    html_attr('title') %>%
  
    # Remove the product families Sale, Bikefinder und schnell verfügbare Bikes
    discard(.p = ~stringr::str_detect(.x,"Sale|Bike Finder|Schnell verfügbare Bikes"))%>%
  
    #Convert vector to tibble
    enframe(name = "position", value ="family_class") %>%
  
  mutate(
    family_id = str_glue("#{family_class}")
  )

bike_family_tbl

# 1.2 --- Create Produt Family URLs of ROSE ----

# Create bike family url tbl to point at right nodes
bike_family_url_tbl <- html_home %>%
  
  #Parent node
  html_nodes(".columns") %>%
  
  #Get the nodes for the families
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  #And extract the information of the href attribute for Bike URLs
  html_attr('href') %>%
  
  # Remove the product families Sale, Bikefinder und schnell verfügbare Bikes
  discard(.p = ~stringr::str_detect(.x,"/fahrräder/sale|/bike-finder|/bikes-mit-kurzer-lieferzeit-rennrad"))%>%
  
  #Convert vector to tibble
  enframe(name = "position", value ="subdirectory") %>%
  
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

bike_family_url_tbl

#1.3 --- Collect Product Categories of Rose ---

#HTML Tags deeply nested + JAVA, thus individual bike category data gathering

url_mtb <- "https://www.rosebikes.de//fahrräder/mtb"
url_rennrad <- "https://www.rosebikes.de/fahrräder/rennrad"
url_ebike <- "https://www.rosebikes.de/fahrräder/e-bike"

bike_category_mtb_tbl <-  read_html(url_mtb) %>%
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"/fahrräder/mtb$"))%>%
  enframe(name = "position", value ="subdirectory") %>% 
  mutate(url = glue("https://www.rosebikes.de{subdirectory}")) %>%
  distinct(url)

bike_category_rennrad_tbl <-  read_html(url_rennrad) %>%
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"/fahrräder/rennrad$"))%>%
  enframe(name = "position", value ="subdirectory") %>% 
  mutate(url = glue("https://www.rosebikes.de{subdirectory}")) %>%
  distinct(url)

bike_category_ebike_tbl <-  read_html(url_ebike) %>%
  html_nodes(css = ".catalog-navigation__list-item > a") %>%
  html_attr('href') %>%
  discard(.p = ~stringr::str_detect(.x,"/fahrräder/e-bike$"))%>%
  enframe(name = "position", value ="subdirectory") %>% 
  mutate(url = glue("https://www.rosebikes.de{subdirectory}")) %>%
  distinct(url)

#In theory you would need another "rest" tibble from all the product families not having product categories, but since this might interfere with the rest of my code due to the different nature of the path, I will not include the rest in the bike category tibble
bike_category_rest_tbl <- bike_family_url_tbl %>%
  filter(!row_number() %in% c(1, 2, 4))

bike_category_tbl <- rbind(bike_category_mtb_tbl,bike_category_rennrad_tbl,bike_category_ebike_tbl)
# 2.0 --- COLLECT BIKE DATA [For a single bike first] ---

#Select first bike pf the first category [mtb] url
bike_category_url <- bike_category_tbl$url[1]
#xopen(bike_category_url)

# Get the URLs for the first bike of the first category
html_bike_category  <- read_html(bike_category_url)
bike_url_tbl        <- html_bike_category %>%
  html_nodes(css = ".align-middle > a") %>%
  html_attr('href') %>%
  enframe(name = "position", value = "url")

#Get the description for the first bike of the first category
bike_desc_tbl <- html_bike_category %>%
  
    html_nodes(".catalog-category-bikes__list-item") %>%
    html_nodes(".catalog-category-bikes__content-subtitle") %>%
    html_text() %>%
    enframe(name = "position", value = "description")

#Stack all lists together  
  

bike_price_tbl <- html_bike_category %>%
  html_nodes (css =".catalog-category-bikes__price-title") %>%
  html_text() %>%
  enframe(name = "position", value = "price")

bike_name_tbl <- html_bike_category %>%
  html_nodes (css = ".basic-headline__title") %>%
  html_text() %>%
  discard(.p = ~stringr::str_detect(.x,"Cross Country|Beratung"))%>%
  enframe(name ="position", value ="name")

single_bike_data_tbl <-  bike_name_tbl %>%
  left_join(bike_price_tbl) %>%
  left_join(bike_desc_tbl) %>%
  left_join(bike_url_tbl)

# 2.2 Wrap it into a function

get_bike_data <- function(url) {
  html_bike_category <- read_html(url)
  
  #Get the URLs
  html_bike_category  <- read_html(bike_category_url)
  bike_url_tbl        <- html_bike_category %>%
    html_nodes(css = ".align-middle > a") %>%
    html_attr('href') %>%
    enframe(name = "position", value = "url")
  
  #Get the descrition
  bike_desc_tbl <- html_bike_category %>%
    
    html_nodes(".catalog-category-bikes__list-item") %>%
    html_nodes(".catalog-category-bikes__content-subtitle") %>%
    html_text() %>%
    enframe(name = "position", value = "description")
  
  #Get the price
  bike_price_tbl <- html_bike_category %>%
    html_nodes (css =".catalog-category-bikes__price-title") %>%
    html_text() %>%
    enframe(name = "position", value = "price")
  
  #Get the name
  bike_name_tbl <- html_bike_category %>%
    html_nodes (css = ".basic-headline__title") %>%
    html_text() %>%
    discard(.p = ~stringr::str_detect(.x,"Cross Country|Beratung"))%>%
    enframe(name ="position", value ="name")
  
  #Stick everything together
  single_bike_data_tbl <-  bike_name_tbl %>%
    left_join(bike_price_tbl) %>%
    left_join(bike_desc_tbl) %>%
    left_join(bike_url_tbl)
}

# Run the function with the first url to check if it is working
#bike_category_url <- bike_category_tbl$url[1]
#bike_data_tbl     <- get_bike_data(url = bike_category_url)

#bike_data_tbl

#2.3 Run for all bike category urls, so we get the data for every bike

bike_data_tbl <- tibble()

# Loop through all urls
for (i in seq_along(bike_category_tbl$url)) {
  
  bike_category_url <- bike_category_tbl$url[i]
  bike_data_tbl     <- bind_rows(bike_data_tbl, get_bike_data(bike_category_url))
  
  # Wait between each request to reduce the load on the server 
  # Otherwise we could get blocked
  Sys.sleep(5)
  
  # print the progress
  print(i)
  
}


bike_data_cleaned_tbl <- bike_data_tbl %>%
  na.omit() %>%
  select(.,-position)

#my_dataframe$column1 = as.numeric(as.character(my_dataframe$column1)) 