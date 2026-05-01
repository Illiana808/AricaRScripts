
library(dbplyr)
library(tidyverse)
library(EnvStats)
library(writexl)

##Doing CF, PLI, and Igeo w/ R SSLs

EPA_Soil_Screen_Troubleshoot <-tribble( #this works
  ~"analyte", ~"screening_level_(mg/kg)",
  "aluminum",	7.70E+03,
  "arsenic",	3.5E00,
  "beryllium",	1.60E+01,
  "vanadium",	3.90E+01,
  "cadmium",	7.10E-01,
  "chromium",	NA,
  "manganese",	1.80E+02,
  "iron",	5.50E+03,
  "cobalt",	2.30E+00,
  "nickel",	8.20E+01,
  "copper",	3.10E+02,
  "zinc",	2.30E+03,
  "selenium",	3.90E+01,
  "molybdenum",	3.90E+01,
  "silver",	3.90E+01,
  "tin",	4.70E+03,
  "antimony",	3.10E+00,
  "barium",	1.50E+03,
  "lead",	1.00E+02,
  "lithium",	1.60E+01,
  "gallium",	NA,
  "rubidium", NA, 
  "strontium",	4.70E+03,
  "indium",	NA,
  "cesium", 	NA,
  "thallium",	7.80E-02,
  "uranium",	1.60E+00)|> print()


CF_2021_EPA <- full_join(Piv_1_Arica_Soil_2021_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(CF = value / `screening_level_(mg/kg)` ) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(CF),  
    SD = sd(CF),
    Median = median(CF),         
    Max = max(CF),               
    Min = min(CF),               
    Geomean = geoMean(CF),
    GeoSD = geoSD(CF),
    Sample_size = n(),
    .groups= "drop") |> print()

CF_2021_round_E <- CF_2021_EPA |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
            ~signif(., digits=3))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
CF_2021_compress_E <- CF_2021_round_E |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

CF_2021_comb_EPA <- CF_2021_compress_E |> 
  select(analyte, `Mean(SD)`, `Med(Min-Max)`, `Geomean(GeoSD)`) |> print()

write_xlsx(CF_2021_comb_EPA,"/Users/illianasamorano/Downloads/CF_2021_EPA.xlsx")

### Igeo 

Igeo_2021_EPA <- full_join(Piv_1_Arica_Soil_2021_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(Igeo = log2((value) / (1.5*`screening_level_(mg/kg)`))) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(Igeo),  
    SD = sd(Igeo),
    Median = median(Igeo),         
    Max = max(Igeo),               
    Min = min(Igeo),
    Sample_size = n(),
    .groups= "drop") |> print()

Igeo_2021_round_E <- Igeo_2021_EPA |> 
  mutate(across(c(Mean, SD, Median, Min, Max), 
                ~ signif(., 3))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Igeo_2021_compress_E <- Igeo_2021_round_E |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |> print()

Igeo_2021_comb_EPA <- Igeo_2021_compress_E |> 
  select(analyte, `Mean(SD)`, `Med(Min-Max)`) |> print()

write_xlsx(Igeo_2021_comb_EPA,"/Users/illianasamorano/Downloads/Igeo_2021_EPA.xlsx")

#### 2022

CF_2022_EPA <- full_join(Piv_1_Arica_Soil_2022_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(CF = value / `screening_level_(mg/kg)` ) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(CF),  
    SD = sd(CF),
    Median = median(CF),         
    Max = max(CF),               
    Min = min(CF),               
    Geomean = geoMean(CF),
    GeoSD = geoSD(CF),
    Sample_size = n(),
    .groups= "drop") |> print()

CF_2022_round_E <- CF_2022_EPA |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
                ~signif(., digits=3))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
CF_2022_compress_E <- CF_2022_round_E |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

CF_2022_comb_EPA <- CF_2022_compress_E |> 
  select(analyte, `Mean(SD)`, `Med(Min-Max)`, `Geomean(GeoSD)`) |> print()

write_xlsx(CF_2022_comb_EPA,"/Users/illianasamorano/Downloads/CF_2022_EPA.xlsx")

### Igeo 

Igeo_2022_EPA <- full_join(Piv_1_Arica_Soil_2022_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(Igeo = log2((value) / (1.5*`screening_level_(mg/kg)`))) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(Igeo),  
    SD = sd(Igeo),
    Median = median(Igeo),         
    Max = max(Igeo),               
    Min = min(Igeo),
    Sample_size = n(),
    .groups= "drop") |> print()

Igeo_2022_round_E <- Igeo_2022_EPA |> 
  mutate(across(c(Mean, SD, Median, Min, Max), 
                ~ signif(., 3))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Igeo_2022_compress_E <- Igeo_2022_round_E |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |> print()

Igeo_2022_comb_EPA <- Igeo_2022_compress_E |> 
  select(analyte, `Mean(SD)`, `Med(Min-Max)`) |> print()

write_xlsx(Igeo_2022_comb_EPA,"/Users/illianasamorano/Downloads/Igeo_2022_EPA.xlsx")