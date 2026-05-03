# Code for making table out of Arica Dust data!


# Importing data for Dust 

library(readxl)
arica_dust2022 <- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Original Gardenroots Data/arica_dust2022.xlsx")
View(arica_dust2022)

library(tidyverse)
library(tibble)
library(dplyr)
library(EnvStats)
library(tidyr)
library(writexl)

# Exceedence of 2023 US EPA Proposed Dust Clearance Level for window trough & floor
library(tibble)
Dust_exceedences_out_in <- tribble(
  ~Analyte, ~threshold, ~type,
  "aluminum", NA, "Outdoor Porch Dust",
  "arsenic", NA, "Outdoor Porch Dust",
  "barium", NA, "Outdoor Porch Dust",
  "beryllium", NA, "Outdoor Porch Dust",
  "cadmium", NA, "Outdoor Porch Dust",
  "chromium", NA, "Outdoor Porch Dust",
  "manganese", NA, "Outdoor Porch Dust",
  "nickel", NA,"Outdoor Porch Dust",
  "zinc", NA, "Outdoor Porch Dust",
  "lead", NA, "Outdoor Porch Dust",
  "copper", NA, "Outdoor Porch Dust",
  "aluminum", NA, "Indoor Floor Dust",
  "arsenic", NA, "Indoor Floor Dust",
  "barium", NA, "Indoor Floor Dust",
  "beryllium", NA, "Indoor Floor Dust",
  "cadmium", NA, "Indoor Floor Dust",
  "chromium", NA, "Indoor Floor Dust",
  "manganese", NA, "Indoor Floor Dust",
  "nickel", NA,"Indoor Floor Dust",
  "zinc", NA, "Indoor Floor Dust",
  "lead", 5, "Indoor Floor Dust",
  "copper", NA, "Indoor Floor Dust") 

#Pivot longer analyte data
Piv_1_Arica_Dust <- 
  tibble(arica_dust2022) |> 
  pivot_longer(cols = aluminum:copper, names_to = "Analyte", values_to = "value") |> print()

# Full table for Arica Dust
Dust_T <- full_join(Piv_1_Arica_Dust, Dust_exceedences_out_in, by = c("Analyte" = "Analyte", "type" = "type"), relationship = "many-to-many") |> 
  mutate(exceedences = ifelse(!is.na(value) & !is.na(threshold), value > threshold, NA)) |> 
  group_by(Analyte,type)|> 
  summarize(
    Mean = mean(value),            
    SD = sd(value),           
    Median = median(value),       
    Max = max(value),               
    Min = min(value),               
    Geomean = geoMean(value),       
    GeoSD = geoSD(value),
    Exceedance_count = sum(exceedences),
    Exceedance_percent = sum(exceedences) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |>print()

# Rounding analyte values * % to 3 dec. pts, keeping counts + sample size whole integers
Dust_T_round <- Dust_T |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD, Exceedance_percent), 
                ~ signif(., 4))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Combine <- Dust_T_round |> 
  mutate(
    `Mean(SD)` = paste(Mean, "(", SD, ")", sep = ""),
    `Med(Min-Max)` = paste(Median, "(", Min, "-", Max, ")", sep = ""),
    `Geomean(GeoSD)` = paste(Geomean, "(", GeoSD, ")", sep = ""),
    `Exceedances(%)` = paste(Exceedance_count, "(", Exceedance_percent, ")", sep = "")
  ) |>
  dplyr::select(type, Analyte, `Mean(SD)`, `Med(Min-Max)`, `Geomean(GeoSD)`, `Exceedances(%)`) |>
  arrange(type, Analyte)

print(Combine)

# Runni



  
  
      
  
      
          







