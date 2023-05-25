#Tidyverse-Script
#Author: Moritz Henkel
# SALES ANALYSIS Working Frame----

## Load libraries ----
library(tidyverse)
library(readxl)
## Importing Files ----
bikes_tbl <- read_excel(path = "Intro to tidyverse/ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("Intro to tidyverse/ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("Intro to tidyverse/ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
## Examining Data ----
glimpse(bikeshops_tbl)
glimpse(orderlines_tbl)
glimpse(bikes_tbl)
## Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
glimpse(bike_orderlines_joined_tbl)
## Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ",") %>%

  mutate(total.price = price * quantity) %>%
  
  select(-...1, -gender,-url,-lat,-lng,-name,-frame.material,-weight,-category,-model,-model.year) %>%
  
  select(order.id, contains("order"), city, state,
         price, quantity, total.price,
         everything()) %>%
  
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Business Insights ----
### Sales by Location ----
## Step 1 - Manipulate
sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
select(total_price, state) %>%
group_by(state) %>%
summarise(sales = sum(total_price)) %>%
ungroup() %>%
mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_location_tbl
## Step 2 - Visualize
sales_by_location_tbl %>%
  
ggplot(aes(x = state, y = sales)) +
geom_col(fill = "#2DC6D6") +
geom_label(aes(label = sales_text)) +
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(
    title    = "Revenue by Location",
    subtitle = "Highest Revenue in NRW",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
### Sales by Location (state) and year (facet_wrap) ----
## Step 1 - Manipulate
sales_by_year_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
select(order_date, total_price, state) %>%
mutate(year = year(order_date)) %>%
  
group_by(year, state) %>%
summarise(sales=sum(total_price)) %>%
ungroup() %>%

mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_by_location_tbl
## Step 2 - Visualize
sales_by_year_by_location_tbl %>%

ggplot(aes(x =year, y = sales, fill = state)) +
  
geom_col() +
geom_smooth(method = "lm", se = FALSE) +
  
facet_wrap(~ state) +

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by state and year",
    subtitle = "NRW has the steepest trend curve, but Hamburg has the most continuous growth",
    fill = "Main category" # Changes the legend name
  ) 
