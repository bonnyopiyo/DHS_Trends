library(readr)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(tidyverse)

setwd("C:/Users/bonfa/Downloads/DHS_Indicators/KDHS_Survey_On_HIV/Data Sets")

Fertility_by_county_2014 <- read_csv("Table 5.2_C  Fertility by county.2014.csv")
Fertility_by_county_2022 <- read_csv("Table 5.2_C  Fertility by county.2022.csv")


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

# Create a gt table
gt_table <- comparison_data %>%
  mutate(TFR_Trend = ifelse(`2022_TFR` > `2014_TFR`, "↗", "↘"),
         Pregnancy_Trend = ifelse(`2022_Percentage of women aged 15-49 currently pregnant` > `2014_women aged 15-49 who are currently pregnant`, "↗", "↘"),
         Children_Trend = ifelse(`2022_Mean number of children for women aged 40-49` > `2014_Mean number of children ever born to women aged 40-49`, "↗", "↘")) %>%
  gt() %>%
  tab_header(
    title = "Comparison of Fertility Indicators (2014 vs. 2022)",
    subtitle = "Change in Fertility Indicators Trends"
  ) %>%
  cols_label(
    County = "County Name",
    `2014_TFR` = "2014",
    TFR_Trend = "Trend",
    `2022_TFR` = "2022",
    `2014_women aged 15-49 who are currently pregnant` = "2014",
    Pregnancy_Trend = "Trend",
    `2022_Percentage of women aged 15-49 currently pregnant` = "2022",
    `2014_Mean number of children ever born to women aged 40-49` = "2014",
    Children_Trend = "Trend",
    `2022_Mean number of children for women aged 40-49` = "2022"
  ) %>%
  cols_align(
    align = "left",
    columns = -c("County")
  ) %>%
  tab_spanner(
    label = "TFR",
    columns = c(`2014_TFR`, `2022_TFR`,TFR_Trend)
  ) %>%
  tab_spanner(
    label = "Pregnancy",
    columns = c(`2014_women aged 15-49 who are currently pregnant`, `2022_Percentage of women aged 15-49 currently pregnant`,Pregnancy_Trend)
  ) %>%
  tab_spanner(
    label = "Children 40-49",
    columns = c(`2014_Mean number of children ever born to women aged 40-49`, `2022_Mean number of children for women aged 40-49`,Children_Trend)
  )

# Add sparklines to the table
gt_table <- gt_table %>%
  tab_style(
    style = cell_borders(
      sides = "left",
      color = "transparent"  # Set background color to transparent
    ),
    locations = cells_body(
      columns = c("County")
    )
  ) %>% 
  tab_options(
    heading.title.font.size = 20,
    table.font.size = 13,
    heading.subtitle.font.size = 12,
    column_labels.font.weight = "bold",
  )

custom_colors <- c("red3", "green4")  # Red for low values, green for high values

# Apply the custom colors to the numeric columns
gt_table <- gt_table %>%
  data_color(
    columns = c(
      `2014_TFR`, 
      `2022_TFR`,
      `2014_women aged 15-49 who are currently pregnant`, 
      `2022_Percentage of women aged 15-49 currently pregnant`,
      `2014_Mean number of children ever born to women aged 40-49`, 
      `2022_Mean number of children for women aged 40-49`
    ),
    palette = custom_colors,
    direction = c("column"),
    
  ) |> 
  opt_table_lines(extent = "all")
gt_table



