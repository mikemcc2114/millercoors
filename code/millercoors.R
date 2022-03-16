library(tidyverse)
library(readxl)
library(here)
library(formattable)
library(treemapify)
library(ggplot2)
library(alluvial)

## items to do
# fix columns to currency format???
# remove "direct material" data - see assignment

# for bottom, unanswered columns, come up with potential savings/ease of use
# category for each and rank to make recommendations

### data import
data_raw <- read_xlsx(here("data", 
                           "MillerCoors MRO Spend Analysis Case Data.xlsx"))

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

###############################################################################
### section 2 - data exploration

spend_sankey_data <- data_clean %>% 
  group_by(`Legacy Company`, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

spend_sankey_diagram <- alluvial(spend_sankey_data, 
                                 freq = spend_sankey_data$total_spend)

count_sankey_data <- data_clean %>% 
  group_by(`Legacy Company`, `Plant Name`) %>% 
  summarise(number_vendors = n_distinct(`Vendor*`))

count_sankey_diagram <- alluvial(count_sankey_data, 
                                 freq = count_sankey_data$number_vendors)

total_spend_by_plant <- data_clean %>% 
  group_by(`Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

treemap_plant <- ggplot(total_spend_by_plant, aes(area = total_spend, 
                                            fill = total_spend, 
                                            label = `Plant Name`)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) 
treemap_plant

# https://r-charts.com/part-whole/treemapify/
  
total_spend_by_vendor <- data_clean %>% 
  group_by(`Vendor*`, `Vendor Name*`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

total_spend_by_sub_category1_by_legacy_company <- data_clean %>% 
  group_by(`Sub Category1`, `Legacy Company`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) 

vendor_count_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(number_vendors = n_distinct(`Vendor*`)) %>% 
  arrange(desc(number_vendors), .by_group = TRUE)

top_ten_vendor_count_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(number_vendors = n_distinct(`Vendor*`)) %>% 
  slice_max(number_vendors, n = 10) %>%  
  arrange(desc(number_vendors), .by_group = TRUE)

spend_and_count_of_plants_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`), 
            number_of_plants = n_distinct(Plant)) %>% 
  mutate(percentage_spend = percent(total_spend / sum(total_spend))) %>% 
  arrange(desc(total_spend), by_group = TRUE)

spend_by_subcategory1_by_plant <- data_clean %>% 
  group_by(`Sub Category1`, Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

top_ten_subcategory1_by_plant <- data_clean %>% 
  group_by(`Plant Name`, `Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) %>% 
  slice_max(total_spend, n = 10) %>% 
  arrange(desc(total_spend), .by_group = TRUE)

treemap_top10 <- ggplot(top_ten_subcategory1_by_plant, 
                        aes(area = total_spend, 
                            fill = total_spend, 
                            label = `Plant Name`)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15) 
treemap_top10

spend_by_subcategory1_by_vendor_by_plant <- data_clean %>% 
  group_by(`Sub Category1`, `Vendor*`, `Vendor Name*`, Plant, `Plant Name`) %>% 
  summarise(total_spend = sum(`Total Cost*`))

###############################################################################
### section 3 - data profiling

# Total number of Sub Category titles (spend classes) engaged, number 
# constituting 80% of the spend (Pareto chart)

spend_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) %>% 
  mutate(percentage_spend = percent(total_spend / sum(total_spend))) %>% 
  arrange(desc(total_spend), by_group = TRUE)

# Total number of Vendors used and number of Vendors required to constitute 80% 
# of the spend

vendor_count <- data_clean %>% 
  summarise(number_of_vendors = n_distinct(`Vendor*`))

percentage_spend_by_vendor <- data_clean %>% 
  group_by(`Vendor*`) %>% 
  summarise(total_spend = sum(`Total Cost*`)) %>% 
  mutate(percentage_spend = percent(total_spend / sum(total_spend))) %>% 
  arrange(desc(total_spend), by_group = TRUE) %>% 
  mutate(cumulative_percentage = percent(cumsum(total_spend) / 
                                           sum(total_spend)))

# Top 10 spend classes (Sub Category1) and amount of spend by category

spend_by_top_ten_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(total_spend = sum(`Total Cost*`), 
            number_vendors = n_distinct(`Vendor*`)) %>% 
  slice_max(total_spend, n = 10) %>% 
  arrange(desc(total_spend), .by_group = TRUE)

treemap_top10 <- ggplot(spend_by_top_ten_subcategory1, 
                        aes(area = total_spend, 
                            fill = total_spend, 
                            label = `Sub Category1`)) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    size = 15)
treemap_top10

# Median number of Vendors for the top 10 Sub Category1

median_number_vendors <- median(spend_by_top_ten_subcategory1$number_of_vendors)

# Number of Vendors by Plant

vendor_count_by_plant <- data_clean %>% 
  group_by(`Legacy Company`, Plant, `Plant Name`) %>% 
  summarise(number_vendors = n_distinct(`Vendor*`)) 

# Examples where different Plants use the same Sub Category1 product and the 
# value of the total spend among Plants in that Sub Category1

# RFI - assumption of savings for this?

# The total spend in a spend category is large for the enterprise but 
# individual buying groups (e.g., Plants) inside the company are not aware of
# the size of the spend or how to leverage spend data to secure better prices 
# for their purchase

# The total number of suppliers is very large, in total and for each spend 
# category.  The company cannot create better spend advantage because they split 
# their spending among many suppliers.

# and opposite - subcategories w/ high supplier power / low # suppliers
vendor_count_and_spend_by_subcategory1 <- data_clean %>% 
  group_by(`Sub Category1`) %>% 
  summarise(number_vendors = n_distinct(`Vendor*`),
            total_spend = sum(`Total Cost*`)) %>% 
  arrange(desc(number_vendors), .by_group = TRUE)

# Individual plants inside the company do business with the same supplier 
# unbeknownst to each other.  As a result, one group’s success in negotiating a
# price for goods or services is done at the expense of the other group’s price.

# The spend categories to focus on to get the best value are unknown … thus we 
# (the company) may be spending our efforts on consolidating the supply base in 
# areas where the spend or relationships don’t matter with respect to overall value.

# Purchase Orders (PO’s) written to the same vendor in the same day for the same
# Plant.  Potential value due to consolidation of inbound shipments.    

# RFI - what is potential savings from consolidation? 10%? use to calculate 
# potential savings
purchase_orders_by_plant_by_day <- data_clean %>% 
  group_by(`Legacy Company`, Plant, `Plant Name`, `Document date`, 
           `Vendor*`) %>% 
  summarise(number_purchases = n_distinct(`Purchasing Document`),
            total_spend = sum(`Total Cost*`)) %>% 
  filter(number_purchases != 1)

# Different breweries (plants) use different suppliers for the same part thus 
#fragmenting spend.  How much was spent with companies with higher prices for 
#the same part?

# potential issue - does this account for price difference b/t repaired new parts?
# do this for each type? OEM vs aftermarket? look at each big one for bad data
savings_by_part <- data_clean %>% 
  group_by(Material) %>% 
  summarise(total_spend = sum(`Total Cost*`),
            number_plants = n_distinct(Plant),
            number_prices = n_distinct(`Price/unit*`),
            max_price = max(`Price/unit*`),
            min_price = min(`Price/unit*`),
            quantity_purchased = sum(`Order quantity`),
            potential_savings = sum(`Total Cost*`) - 
              (sum(`Order quantity`) * min(`Price/unit*`))) %>% 
  arrange(desc(potential_savings), .by_group = TRUE) %>% 
  mutate(cumulative_savings = cumsum(potential_savings))

# The spend with a supplier for a high-value part or for a high volume of the 
# same part is significant.  Is there an opportunity to have the part stocked 
# by the supplier (VMI) vs company-purchased and stored?
  
# Are there high-volume or high-value parts (e.g., repaired/spare rotables) 
# that might be pooled or shared rather than held by each plant?

# Are there plants spending more money on OEM parts while others purchase 
# cheaper aftermarket parts? Shift spending to aftermarket to save $?
  
spend_by_OEM <- data_clean %>% 
  group_by(Plant, `OEM / Aftermarket`) %>% 
  summarise(total_spend = sum(`Total Cost*`))
  
  summarise(total_spend = sum(`Total Cost*`),
            number_plants = n_distinct(Plant),
            number_prices = n_distinct(`Price/unit*`),
            max_price = max(`Price/unit*`),
            min_price = min(`Price/unit*`),
            quantity_purchased = sum(`Order quantity`),
            potential_savings = sum(`Total Cost*`) - 
              (sum(`Order quantity`) * min(`Price/unit*`))) %>% 
  arrange(desc(potential_savings), .by_group = TRUE) %>% 
  mutate(cumulative_savings = cumsum(potential_savings))
