library(tidyverse)
library(readxl)
library(here)

## items to do
## fix columns to currency format???
## remove "direct material" data - see assignment

# import data
data <- read_xlsx(here("data", "MillerCoors MRO Spend Analysis Case Data.xlsx"))

# data manipulation
total_spend_by_plant <- data %>% 
  group_by(Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))
  
total_spend_by_vendor <- data %>% 
  group_by(`Vendor*`, `Vendor Name*`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

total_spend_by_sub_category1_by_legacy_company <- data %>% 
  group_by(`Sub Category1`, `Legacy Company`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

vendor_count_by_subcategory1 <- data %>% 
  group_by(`Sub Category1`) %>% 
  summarise(number_of_vendors = n_distinct(`Vendor*`))

spend_and_count_of_plants_by_subcategory1 <- data %>% 
  group_by(`Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`), 
            number_of_plants = n_distinct(Plant))

spend_by_subcategory1_by_plant <- data %>% 
  group_by(`Sub Category1`, Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))


