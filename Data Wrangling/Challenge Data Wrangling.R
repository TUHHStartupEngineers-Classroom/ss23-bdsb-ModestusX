# 1.0 Libraries ---
library(tidyverse)
library(vroom)
library(data.table)
library(tictoc)

# 2.0 DATA IMPORT Question 1 [assignee, patent_assignee] ----

col_types <- list(
  id = col_character(),
  type = col_integer(),
  organization = col_character()
)
assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
) 

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)
patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 3.0 Convert to data.table

setDT(assignee_tbl)
class(assignee_tbl)

setDT(patent_assignee_tbl)
class(patent_assignee_tbl)

# 4.0 DATA WRANGLING

## 4.1 Rename assignee_tbl 

assignee_tbl %>% setnames("id","assignee_id")
assignee_tbl %>% glimpse

## 4.2 Merging Data

tic()
combined_data <- merge(x = assignee_tbl, y = patent_assignee_tbl, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()
combined_data %>% glimpse()

## 4.3 Convert patent_id to numeric

combined_data[, ("patent_id") := lapply(.SD, as.numeric), .SDcols = "patent_id"]
str(combined_data)

###Question 1: Patent Dominance: What US company / corporation has the most patents? 
###List the 10 US companies with the most assigned/granted patents.


patent_dominance <- combined_data[, .N, by = organization]

patent_dominance %>%
  na.omit %>%
  arrange(desc(N)) %>%
  head(10)

###Recent patent activity: What US company had the most patents granted in August 2014? 
###List the top 10 companies with the most new granted patents for August 2014.

col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)
patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

patent_tbl %>% setnames("id","patent_id")
patent_tbl %>% glimpse()
setDT(patent_tbl)
class(patent_tbl)
class(combined_data)

## 4.4 Convert patent_id to numeric

patent_tbl[, ("patent_id") := lapply(.SD, as.numeric), .SDcols = "patent_id"]
str(combined_data)
patent_tbl %>% glimpse


## Merge Data

combined_data_2 <- combined_data[patent_tbl, on = "patent_id"] %>% 
  separate(
  col = "date",
  into = c("year", "month", "day"),
  sep = "-",
  remove = TRUE)

combined_data_2 %>% glimpse()
class(combined_data_2)
setDT(combined_data_2)

#Number of claims
patent_dominance_august <- combined_data_2[month == "08", sum(num_claims), by = organization] 

patent_dominance_august %>%
  na.omit %>% 
  arrange(desc(V1)) %>%
  head(10)

#Number of new patents
patent_dominance_august_2 <- combined_data_2[month == "08", .N, by = organization] 

patent_dominance_august_2 %>%
  na.omit %>% 
  arrange(desc(N)) %>%
  head(10)

## Question 3: What is the most innovative tech sector? 
#For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?

col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL"))

setDT(uspc_tbl)
class(uspc_tbl)


combined_data_3 <- combined_data_2[uspc_tbl, on = "patent_id"] 
combined_data_3[, ("mainclass_id") := lapply(.SD, as.numeric), .SDcols = "mainclass_id"]
clasS(combined_data_3)


tic()
patent_dominance_by_uspc <- combined_data_3[, .N, by = .(organization, mainclass_id)] %>%
  na.omit %>%
  unique() %>%
  arrange(desc(N)) %>%
  head(10)
toc()

patent_dominance_by_uspc

