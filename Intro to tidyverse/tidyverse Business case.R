# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
# 2.0 Importing Files ----
bikes_tbl <- read_excel("ds_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("ds_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("ds_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----
bikes_tbl
orderlines_tbl

glimpse(bikes_tbl)
# 4.0 Joining Data ----


# 5.0 Wrangling Data ----


# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

# Step 2 - Visualize


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

# Step 2 - Visualize



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----
