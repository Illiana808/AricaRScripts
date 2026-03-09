## New 2021 Soil Stats (no need to correct yay)

library(dbplyr)
library(tidyverse)
library(EnvStats)
library(writexl)

# Import 2021 Soil Data from Excel (file path from Illiana Samorano Box folder)
library(readxl)
Arica_Soil_Table_Redone_2021 <- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx", 
                                      sheet = "Soil 2021 data for analysis")
View(Arica_Soil_Table_Redone_2021)


# Pivot to have analyte column for soil 2021
Piv_1_Arica_Soil_2021_redo <- 
  tibble(Arica_Soil_Table_Redone_2021) |> 
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value") |> print()

#source: Regional Screening Level (RSL) Resident Soil Table (TR=1E-06, HQ=0.1) November 2024;
#for consistency, I used Non-cancer Child Hazard at Target Hazard Quotient of 0.1, cuz there's non-cancer values for more of these
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


#Troubleshoot
Soil_2021 <- full_join(Piv_1_Arica_Soil_2021_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(exceedences = ifelse(!is.na(value) & !is.na(`screening_level_(mg/kg)`), value >  `screening_level_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
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
    .groups= "drop") |> print()

### Rounding analyte values & % to 2 dec. pts, keeping counts + sample size whole integers
Soil_2021_round <- Soil_2021 |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD, Exceedance_percent), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Soil_2021_compress <- Soil_2021_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> 
  mutate("Exceedances(%)" = paste(Exceedance_count, "(", Exceedance_percent, ")", sep = ""))|> print()


### 2022 Soil Stats check

##import 2022 soil data from Box
  library(readxl)
Arica_Soil_2022_redone<- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx", 
                                      sheet = "Corrected 2022 Soil values")
View(Arica_Soil_2022_redone)


Piv_1_Arica_Soil_2022_redo <- 
  tibble(Arica_Soil_2022_redone) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "analyte", values_to = "value") |> print()


Soil_2022_redo <- full_join(Piv_1_Arica_Soil_2022_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  filter(!is.na(value)) |>
  mutate(exceedences = ifelse(!is.na(value) & !is.na( `screening_level_(mg/kg)`), value >  `screening_level_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
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
    .groups= "drop") |> print()

## check spelling & fix sig figs
### Rounding analyte values & % to 2 dec. pts, keeping counts + sample size whole integers
Soil_2022_redo_round <- Soil_2022_redo |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD, Exceedance_percent), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Soil_2022_compress <- Soil_2022_redo_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> 
  mutate("Exceedances(%)" = paste(Exceedance_count, "(", Exceedance_percent, ")", sep = ""))|> print()

##export & fix old tables; do stats testing for vanadium

## combining tables w/ formatting 
Soil_Detailed_with_format <- bind_rows(
  Soil_2021_compress |> mutate(Year = "2021"),
  Soil_2022_compress |> mutate(Year = "2022")
) |> 
  select(Year, analyte, `Mean(SD)`, `Med(Min-Max)`, `Geomean(GeoSD)`, 
         `Exceedances(%)`, Sample_size) |> 
  arrange(analyte, Year) |> 
  print()

write_xlsx(Soil_Detailed_with_format, "/Users/illianasamorano/Downloads/Soil202122descriptivestats.xlsx")



