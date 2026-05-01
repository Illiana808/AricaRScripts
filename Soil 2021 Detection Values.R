## First check ICP-MS 2021 Soil Values for non-detect values
#Note: MDL stands for method detection limit

library(tidyverse)
library(tibble)
library(dplyr)
library(EnvStats)
library(tidyr)
library(writexl)

##Import data for Soil 2021 (raw ICP-MS values)
# Import 2021 Soil Data from Excel (file path from Illiana Samorano Box folder)
library(readxl)
Arica_Soil_Table_Redone_2021 <- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx", 
                                           sheet = "Soil 2021 data for analysis")
View(Arica_Soil_Table_Redone_2021)


##Import 2021 Soil MDLs for comparison
library(readxl)
Arica_Soil_Table_MDLS <- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx", 
                                      sheet = "2021 Soil MDLs")
View(Arica_Soil_Table_MDLS)

Piv_1_Arica_Soil_2021_Counts <- 
  tibble(Arica_Soil_Table_Redone_2021) |> 
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value") |> print()

# Soil 2021 Detection Values
Soil_2021_Detection <- full_join(Arica_Soil_Table_MDLS, Piv_1_Arica_Soil_2021_Counts, by = c("analyte" = "analyte"))|>
  mutate(detection_number =ifelse(!is.na(value) & !is.na("MDL"), value >= MDL, NA)) |> 
  group_by(analyte)|> 
  summarize(
    Detection_count = sum(detection_number),
    Detection_percent = sum(detection_number) / n() * 100,
    Sample_size = n(),
    .groups= "drop")  |> print()




