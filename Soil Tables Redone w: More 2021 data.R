## New 2021 Soil Stats (no need to correct yay)

library(dbplyr)
library(tidyverse)
library(EnvStats)
library(writexl)

# Import 2021 Soil Data from Excel (alr in environment, but here for good practice)
library(readxl)
Arica_Soil_Table_Redone_Soil_2021 <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                                sheet = "Soil 2021 data for analysis") |>print()




view(Arica_Soil_Table_Redone_Soil_2021)

# Pivot to have analyte column for soil 2021
Piv_1_Arica_Soil_2021_redo <- 
  tibble(Arica_Soil_Table_Redone_Soil_2021) |> 
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value") |> print()

#Import EPA soil reference values (Regional Screening Level (RSL) Summary Table (TR=1E-06, HQ=0.1) November 2024)
library(readxl) #bad bad excel file sad
EPA_Soil_Screen_ext <- read_excel("~/Downloads/Geochem table for soil.xlsx", 
                                     sheet = "US EPA values check") |> view()

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

write_xlsx(EPA_Soil_Screen_Troubleshoot,"/Users/illianasamorano/Downloads/EPA_Soil_Screening_lvls.xlsx")


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
    .groups= "drop") |> view()



#Full table for Arica Soil error w/ exceedences(excel didn't perfectly line up sad)
#Soil_2021 <- full_join(Piv_1_Arica_Soil_2021_redo, EPA_Soil_Screen_ext, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  #mutate(exceedences = ifelse(!is.na(value) & !is.na(`screening level (mg/kg)`), value >  `screening level (mg/kg)`, NA)) |> 
  #group_by(analyte)|> 
  #summarize(
   # Mean = mean(value),  
   # SD = sd(value),
    #Median = median(value),         
   # Max = max(value),               
    #Min = min(value),               
  #  Geomean = geoMean(value),
#    GeoSD = geoSD(value),
  #  Exceedance_count = sum(exceedences),
  #  Exceedance_percent = sum(exceedences) / n() * 100,
   # Sample_size = n(),
    # .groups= "drop") |> view()


## check spelling & fix sig figs
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

##just delete extra columns

write_xlsx(Soil_2021_compress,"/Users/illianasamorano/Downloads/Soil_2021_compress_NC.xlsx")


----------------
### 2022 Soil Stats check

##import 2022 soil data
library(readxl)
Arica_Soil_2022_redone <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                      sheet = "Corrected 2022 Soil values") |> print()
Piv_1_Arica_Soil_2022_redo <- 
  tibble(Arica_Soil_2022_redone) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "analyte", values_to = "value") |> print()


Soil_2022_redo <- full_join(Piv_1_Arica_Soil_2022_redo, EPA_Soil_Screen_Troubleshoot, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
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
  mutate("Exceedances(%)" = paste(Exceedance_count, "(", Exceedance_percent, ")", sep = ""))|> view()

##just delete extra columns

##export & fix old tables; do stats testing for vanadium

write_xlsx(Soil_2022_compress,"/Users/illianasamorano/Downloads/Soil_2022_compress_NC.xlsx")









