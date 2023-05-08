library(tidyverse)
library(ggplot2) # To load the diamonds dataset
library(dplyr)
diamonds2 <- readRDS("diamonds2.rds")

diamonds2

diamonds2 %>% head(n = 5)
## # A tibble: 5 x 3
##   cut     `2008` `2009`
##   <chr>    <dbl>  <dbl>
## 1 Ideal      326    332
## 2 Premium    326    332
## 3 Good       237    333
## 4 Premium    334    340
## 5 Good       335    341

diamonds2 %>% 
  pivot_longer(cols      = c("2008", "2009"), 
               names_to  = 'year', 
               values_to = 'price') %>% 
  head(n = 5)

diamonds3 <- readRDS("diamonds3.rds")

#Example3
diamonds3 %>% head(n = 5)
## # A tibble: 5 x 5
##   cut     price clarity dimension measurement
##   <ord>   <dbl> <ord>   <chr>           <dbl>
## 1 Ideal     326 SI2     x                3.95
## 2 Premium   326 SI1     x                3.89
## 3 Good      327 VS1     x                4.05
## 4 Ideal     326 SI2     y                3.98
## 5 Premium   326 SI1     y                3.84

diamonds3 %>% 
  pivot_wider(
    names_from = "dimension",
    values_from = "measurement"
  ) %>%
  head(n=5)

#Example 4
diamonds4 <- readRDS("diamonds4.rds")

diamonds4
## # A tibble: 5 x 4
##   cut     price clarity dim           
##   <ord>   <dbl> <ord>   <chr>         
## 1 Ideal     326 SI2     3.95/3.98/2.43
## 2 Premium   326 SI1     3.89/3.84/2.31
## 3 Good      327 VS1     4.05/4.07/2.31
## 4 Premium   334 VS2     4.2/4.23/2.63 
## 5 Good      335 SI2     4.34/4.35/2.75

diamonds4 %>% separate(
  col= dim, 
  into = c("x","y","z"),
  sep = "/",
  convert = T)

#Example 5
diamonds5 <- readRDS("diamonds5.rds")

diamonds5
## # A tibble: 5 x 7
##   cut     price clarity_prefix clarity_suffix     x     y     z
##   <ord>   <dbl> <chr>          <chr>          <dbl> <dbl> <dbl>
## 1 Ideal     326 SI             2               3.95  3.98  2.43
## 2 Premium   326 SI             1               3.89  3.84  2.31
## 3 Good      327 VS             1               4.05  4.07  2.31
## 4 Premium   334 VS             2               4.2   4.23  2.63
## 5 Good      335 SI             2               4.34  4.35  2.75

diamonds5 %>% 
  unite(clarity, clarity_prefix, clarity_suffix, sep = '')

diamonds
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)

diamonds %>% 
  arrange(cut, carat, desc(price))