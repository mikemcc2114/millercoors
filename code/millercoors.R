library(tidyverse)
library(readxl)
library(here)
library(s)
library(DataExplorer)

## items to do
## fix columns to currency format???
## remove "direct material" data - see assignment

### data import
data_raw <- read_xlsx(here("data", "MillerCoors MRO Spend Analysis Case Data.xlsx"))

### data cleaning

# remove direct material to focus on maintenance, repair, and operations (MRO)

direct_material <- c("SHIPMTL", "XB-CARR", "XB-CRTN", "XB-CRWN", "XB-LBL-B",
                     "XB-LBL-N", "XB-SLVE", "XBTL", "XB-TRAY", "XCAN", 
                     "XCAN-ABTL", "XC-CONE", "XC-DIV", "XC-END", "XC-TRAY",
                     "XKEG-DEP", "XKEG-DUST", "XOTHER")

data_clean <- data_raw %>%
  filter(!`Material group` %in% direct_material)

# number of direct material items removed = 
length(data_raw$`Material group`) - length(data_clean$`Material group`)

# initial data exploration
create_report(data_clean)

## case questions - data manipulation
total_spend_by_plant <- data_clean %>% 
  group_by(Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))
  
total_spend_by_vendor <- data_clean %>% 
  group_by(`Vendor*`, `Vendor Name*`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

total_spend_by_sub_category1_by_legacy_company <- data_clean %>% 
  group_by(`Sub Category1`, `Legacy Company`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) 

vendor_count_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(number_of_vendors = n_distinct(`Vendor*`)) 

spend_and_count_of_plants_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`), 
            number_of_plants = n_distinct(Plant)) %>% 

spend_by_subcategory1_by_plant <- data_clean %>% 
  group_by(`Sub Category1`, Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))
  

top_ten_subcategory1_by_plant <- data_clean %>% 
  group_by(Plant, `Plant Name`, `Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) %>% 
  slice_max(total_spend, n = 10) %>% 
  arrange(desc(total_spend), .by_group = TRUE)


