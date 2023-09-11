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

df <- comparison_data %>%
  setNames(c("County", "2014_TFR", "2014_PREG", "2014_CHILD", "2022_TFR", "2022_PREG",
             "2022_CHILD")) %>% 
  rowwise() %>% 
  mutate(change_TFR = list(c(`2014_TFR`, `2022_TFR`)),
         change_PREG = list(c(`2014_PREG`, `2022_PREG`)),
         change_CHILD = list(c(`2014_CHILD`, `2022_CHILD`))
  ) %>% 
  select(County, `2014_TFR`, `2022_TFR`, change_TFR, 
         `2014_PREG`, `2022_PREG`, change_PREG,
         `2014_CHILD`, `2022_CHILD`, change_CHILD
  )


glimpse(comparison_data)

s <- "black"
fv <- "black"
rl <- "black"
rh <- "black"
t <- "black"
          
c_p <- c(s, fv, rl, rh, t)
        
table <- df %>% 
          as_tibble() %>% 
          gt() %>% 
          gt_plt_sparkline(change_TFR, label = F, fig_dim = c(4, 8), palette = c_p) %>% 
          gt_plt_sparkline(change_PREG, label = F, fig_dim = c(4, 8), palette = c_p) %>% 
          gt_plt_sparkline(change_CHILD, label = F, fig_dim = c(4, 8), palette = c_p) %>% 
          tab_header(
            title = "Comparison of Fertility Indicators (2014 vs. 2022)",
            subtitle = "Change in Fertility Indicators Trends"
          ) %>% 
          tab_spanner(
            label = "Total Fertility Rate",
            columns = `2014_TFR` : `2022_TFR`
          )%>% 
          tab_spanner(
            label = "Pregnancy (15-49)",
            columns = `2014_PREG` : `2022_PREG`
          )%>% 
          tab_spanner(
            label = "Children 40-49",
            columns = `2014_CHILD` : `2022_CHILD`
          ) %>% 
          cols_label(
            County = "County Name",
            `2014_TFR` = "2014",
            `2022_TFR` = "2022",
            change_TFR = "Trend",
            `2014_PREG` = "2014",
            `2022_PREG` = "2022",
            change_PREG = "Trend",
            `2014_CHILD` = "2014",
            `2022_CHILD` = "2022",
            change_CHILD = "Trend"
            
          ) %>% 
          cols_align("left") %>% 
          cols_width(
            County ~ px(130),
            `2014_TFR` ~ px(50),
            `2022_TFR` ~ px(50),
            change_TFR ~ px(50),
            `2014_PREG` ~ px(50),
            `2022_PREG` ~ px(50),
            change_PREG ~ px(50),
            `2014_CHILD` ~ px(50),
            `2022_CHILD` ~ px(50),
            change_CHILD ~ px(50)
          )
        
        custom_colors <- c("red3", "green4")
        
        table <- table %>% 
          data_color(
            columns = c(
              `2014_TFR`,
              `2022_TFR`,
              `2014_PREG`,
              `2022_PREG`,
              `2014_CHILD`,
              `2022_CHILD`
            ),
            palette = custom_colors,
            direction = "column"
          ) %>% 
          opt_table_lines(extent = "all") |> 
          tab_options(
            heading.title.font.size = 20,
            table.font.size = 10,
            heading.subtitle.font.size = 12,
            column_labels.font.weight = "bold",
          )
          
print(table)        
