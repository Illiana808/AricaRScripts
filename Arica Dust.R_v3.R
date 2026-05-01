# Code for making table out of Arica Dust data!
# Importing data for Dust - done
library(readxl)
Arica_Dust_file_for_R_Indoor_Outdoor <- read_excel("~/Downloads/Arica Dust file for R_Indoor & Outdoor.xls", 
                                                   sheet = "Arica Dust file for R")
View(Arica_Dust_file_for_R_Indoor_Outdoor)

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
  "lead", 40, "Outdoor Porch Dust",
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
  tibble(Arica_Dust_file_for_R_Indoor_Outdoor) |> 
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
    .groups= "drop") |>view()

# Rounding analyte values * % to 3 dec. pts, keeping counts + sample size whole integers
Dust_T_round <- Dust_T |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD, Exceedance_percent), 
                ~ round(., 3))) 

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Dust_comb <- Dust_T_round |> 
  unite(Mean_SD, c(Mean, SD)) |>
  unite("Med_Min_Max", c(Median, Min, Max)) |>
  unite("Geomean_GeoSD", c(Geomean, GeoSD)) 

#Pivot Wider table for Arica Dust (to make separate columns for outdoor & indoor dust values)
Dust_T_sep_in_out <- Dust_comb  |>
  pivot_wider(
    names_from = type,
    values_from = c(Mean_SD, Med_Min_Max, Geomean_GeoSD, Exceedance_count, Exceedance_percent, Sample_size),
    names_sep = "_") 


#Manually reordering columns so indoor & outdoor dust stats can be together
reorder_man_dust = c("Analyte", "Mean_SD_Indoor Floor Dust", "Geomean_GeoSD_Indoor Floor Dust", "Med_Min_Max_Indoor Floor Dust","Exceedance_count_Indoor Floor Dust", "Exceedance_percent_Indoor Floor Dust", 
                "Sample_size_Indoor Floor Dust",
                 "Mean_SD_Outdoor Porch Dust", "Geomean_GeoSD_Outdoor Porch Dust", "Med_Min_Max_Outdoor Porch Dust","Exceedance_count_Outdoor Porch Dust", "Exceedance_percent_Outdoor Porch Dust",
                "Sample_size_Outdoor Porch Dust")
reorder_Dust <- Dust_T_sep_in_out[, reorder_man_dust] 

reorder_Dust |>print()


write_xlsx(reorder_Dust,"/Users/illianasamorano/Downloads/Dust_Stats_from_R.xlsx")






  
  
      
  
      
          







