library(readr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)

setwd("C:/Users/bonfa/Downloads/DHS_Indicators/KDHS_Survey_On_HIV/Data Sets")

Fertility_by_county_2014 <- read_csv("C:/Users/bonfa/Downloads/Table 5.2_C  Fertility by county.2014.csv")
Fertility_by_county_2022 <- read_csv("C:/Users/bonfa/Downloads/Table 5.2_C  Fertility by county.2022.csv")

# Pivot the 2014 data with meaningful column names
pivot_2014 <- Fertility_by_county_2014 %>%
  pivot_wider(
    names_from = Category,
    values_from = Proportion,
    names_prefix = "2014_" 
  )

# Pivot the 2022 data with meaningful column names
pivot_2022 <- Fertility_by_county_2022 %>%
  pivot_wider(
    names_from = Category,
    values_from = Proportion,
    names_prefix = "2022_" 
  )

# Standardize specific county names in pivot_2014
pivot_2014 <- pivot_2014 %>%
  mutate(
    County = case_when(
      County == "Elgeyo Marakwet" ~ "Elgeyo Marakwet",
      County == "Trans-Nzoia" ~ "Trans Nzoia",
      County == "Nairobi" ~ "Nairobi",
      County == "Taita Taveta" ~ "Taita Taveta",  # Standardize Taita Taveta
      TRUE ~ County  # Leave other counties unchanged
    )
  )

# Standardize specific county names in pivot_2022
pivot_2022 <- pivot_2022 %>%
  mutate(
    County = case_when(
      County == "Elgeyo/Marakwet" ~ "Elgeyo Marakwet",
      County == "Trans Nzoia" ~ "Trans Nzoia",
      County == "Nairobi City" ~ "Nairobi",
      County == "Taita/Taveta" ~ "Taita Taveta",  # Standardize Taita Taveta
      TRUE ~ County  # Leave other counties unchanged
    )
  )

# Merge the pivoted data
comparison_data <- full_join(pivot_2014, pivot_2022, by = "County")

# Write the merged dataset to a CSV file
write_csv(comparison_data, "C:/KDHS/data/fertility_data_by_county_2014_2022.csv")
