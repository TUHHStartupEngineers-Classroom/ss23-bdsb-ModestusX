library(tidyverse)
library(dplyr)
# Read the COVID-19 data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

#Data Manipulation
selected_countries <- c("Germany", "United Kingdom", "France", "Spain", "United States")
covid_plot_tbl <- covid_data_tbl %>% 
  select(continent,total_cases,date,location) %>% 
  relocate(continent,location,everything()) %>%
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) %>%
  unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
  mutate(order_date_united = as.Date(order_date_united)) %>%
  na.omit() %>%
  filter(location %in% selected_countries)

# Calculate cumulative cases per month for each country
cumulative_cases <- covid_plot_tbl %>%
  group_by(location, order_date_united) %>%
  summarise(cumulative_cases = sum(total_cases))

# Set colors for each country
colors <- c("Germany" = "purple", "United Kingdom" = "green", "France" = "orange", "Spain" = "pink", "United States" = "yellow")

# Create the line plot
cumulative_cases %>% 
  ggplot(aes(x = order_date_united, y = cumulative_cases, color = location)) +
    geom_line(size = 1) +
    scale_color_manual(values = colors) +
    theme_light()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6, 
                                                    prefix = "", 
                                                    suffix = "M"))+
  labs(
    title = "Covid-19 confirmed cases worldwide",
    subtitle = "As of 25.05.2023",
    x = "",
    y = "Cumulative Cases",
    color = "Continent/Country"
  )
  

