# Getting Detection #s for soil
# Import Raw 2021 Soil ICP-MS  Data 
# Importing data for Dust - done
Arica_Soil_ICPMS_Detection <- read_excel("~/Downloads/Arica Detection #s.xlsx", 
                                   sheet = "Reformatting for R")
print(Arica_Soil_ICPMS_Detection)

library(tidyverse)
library(tibble)
library(dplyr)
library(EnvStats)
library(tidyr)
library(writexl)


ICP_MS_MDL_Soil <-tribble( #MDL stands for method detection limit, calculated by ALEC for each element)
  ~Analyte, ~MDL,
  "beryllium",	0.000,
  "aluminum",	0.200,
  "chromium",	0.191,
  "manganese",	0.015,
  "nickel",	0.040,
  "copper",	0.018,
  "zinc",	0.045,
  "arsenic",	0.227,
  "cadmium",	0.012,
  "barium",	0.021,
  "lead",	0.004) |> print()

#Pivot longer analyte data
Piv_1_Arica_Soil_Detection <- 
  tibble(Arica_Soil_ICPMS_Detection) |> 
  pivot_longer(cols = beryllium:lead, names_to = "Analyte", values_to = "value") |> print()

# Full table for Arica Dust
Soil_Detection <- full_join(Piv_1_Arica_Soil_Detection, ICP_MS_MDL_Soil, by = c("Analyte" = "Analyte"))|>
  mutate(detection_number =ifelse(!is.na(value) & !is.na("MDL"), value > MDL, NA)) |> 
  group_by(Analyte)|> 
  summarize(
    Detection_count = sum(detection_number),
    Detection_percent = sum(detection_number) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |> print()

# Just downloaded as excel & paste detection columns into table

write_xlsx(Soil_Detection,"/Users/illianasamorano/Downloads/Soil_Detection.xlsx")