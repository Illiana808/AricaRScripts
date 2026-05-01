# Getting Detection #s for dust
# Import Raw Dust Data 
# Importing data for Dust - done
library(readxl)
Arica_Dust_Detection <- read_excel("~/Downloads/Arica Dust Detection.xlsx", 
                                   sheet = "Sheet2")
print(Arica_Dust_Detection)

library(tidyverse)
library(tibble)
library(dplyr)
library(EnvStats)
library(tidyr)
library(writexl)


ICP_MS_MDL <-tribble( #MDL stands for method detection limit, calculated by ALEC for each element)
  ~Analyte, ~MDL,
  "beryllium",	0.001,
  "aluminum",	0.146,
  "chromium",	0.005,
  "manganese",	0.001,
  "nickel",	0.002,
  "copper",	0.001,
  "zinc",	0.002,
  "arsenic",	0.003,
  "cadmium",	0.0001,
  "barium",	0.007,
  "lead",	0.0003599) 

#Pivot longer analyte data
Piv_1_Arica_Dust_Detection <- 
  tibble(Arica_Dust_Detection) |> 
  pivot_longer(cols = beryllium:lead, names_to = "Analyte", values_to = "value") |> print()

# Full table for Arica Dust
Dust_Detection <- full_join(Piv_1_Arica_Dust_Detection, ICP_MS_MDL, by = c("Analyte" = "Analyte"))|>
  mutate(detection_number =ifelse(!is.na(value) & !is.na("MDL"), value >= MDL, NA)) |> 
  group_by(Analyte)|> 
  summarize(
    Detection_count = sum(detection_number),
    Detection_percent = sum(detection_number) / n() * 100,
    Sample_size = n(),
    .groups= "drop")  |> print()

# Just downloaded as excel & paste detection columns into table

write_xlsx(Dust_Detection,"/Users/illianasamorano/Downloads/Dust_Detection.xlsx")
 
