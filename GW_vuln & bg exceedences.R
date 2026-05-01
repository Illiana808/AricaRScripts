## Import Data for 2021 Soil

library(readxl)
Arica_Soil_Table_Redone_Soil_2021 <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                                sheet = "Soil 2021 data for analysis") |>print()

# Pivot to have analyte column for soil 2021
Piv_1_Arica_Soil_2021_redo <- 
  tibble(Arica_Soil_Table_Redone_Soil_2021) |> 
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value") |> print()

##Create Tibbles w/ Reference Values; updated w/ values to this specific table, not EPA summary table
#note had to use MCL-based for values where risk-based wasn't avaiable
#MCL based=> total chromium; lead & compounds
EPA_Soil_GWVuln <-tribble(
~"analyte",	~"risk-based_ssl_(mg/kg)",
"beryllium", 2.00E+00,
"aluminum",	3.00E+03,
"arsenic",	1.50E-03,
"cadmium",	1.40E-02,
"chromium",	1.8E05,
"copper",	2.80E+00,
"lead",	9.00E+00,
"manganese",	2.80E+00,
"nickel",	3.20E+00,
"zinc",	3.70E+01,
"vanadium",	8.60E+00,
"iron",	3.50E+01,
"cobalt",	2.70E-02,
"selenium",	5.20E-02,
"molybdenum",	2.00E-01,
"silver",	8.00E-02,
"tin",	3.00E+02,
"antimony",	3.50E-02,
"barium",	1.60E+01,
"lithium",	1.20E+00,
"rubidium",	NA,
"gallium",	NA,
"strontium",	4.20E+01,
"indium",	NA,
"cesium", 	NA,
"thallium",	1.40E-03,
"uranium",	1.80E-01) |> print()

write_xlsx(EPA_Soil_GWVuln,"/Users/illianasamorano/Downloads/GW_vuln_refs.xlsx")


#Known natural background levels
Bg_levels_soil_Arica <-tribble(
~"analyte",	~"bg_level_(mg/kg)",
"copper",	25,
"arsenic",	18.35,
"chromium", 73.63, 
"cadmium",	1.134,
"nickel", 600,
"zinc",	71,
"lead",	11.89) |>print()


##Exceedance of groundwater vuln ref values 2021
Soil_2021_GW_vuln <- full_join(Piv_1_Arica_Soil_2021_redo, EPA_Soil_GWVuln, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(exceedences = ifelse(!is.na(value) & !is.na(`risk-based_ssl_(mg/kg)`), value >  `risk-based_ssl_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
  summarize(
    Exceedance_count = sum(exceedences),
    Exceedance_percent = sum(exceedences) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |> print()


write_xlsx(Soil_2021_GW_vuln,"/Users/illianasamorano/Downloads/Soil_2021_GW_vuln_corr.xlsx")

#Exceedance of bg levels soil 2021
Soil_2021_bg_excd <- full_join(Piv_1_Arica_Soil_2021_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(exceedences = ifelse(!is.na(value) & !is.na(`bg_level_(mg/kg)`), value >  `bg_level_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
  summarize(
    Exceedance_count = sum(exceedences),
    Exceedance_percent = sum(exceedences) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |> print()


write_xlsx(Soil_2021_bg_excd,"/Users/illianasamorano/Downloads/Soil_2021_bg_excd_corr.xlsx")

#Exceedence of GW vuln 2022
library(readxl)
Arica_Soil_2022_redone <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                     sheet = "Corrected 2022 Soil values") |> print()
Piv_1_Arica_Soil_2022_redo <- 
  tibble(Arica_Soil_2022_redone) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "analyte", values_to = "value") |> print()

Soil_2022_GW_vuln <- full_join(Piv_1_Arica_Soil_2022_redo, EPA_Soil_GWVuln, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(exceedences = ifelse(!is.na(value) & !is.na(`risk-based_ssl_(mg/kg)`), value >  `risk-based_ssl_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
  summarize(
    Exceedance_count = sum(exceedences),
    Exceedance_percent = sum(exceedences) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |> view()
##makes sense to  have many rows w/ 1 sample size--> cuz xrf didnt analyze for those that yr

write_xlsx(Soil_2022_GW_vuln,"/Users/illianasamorano/Downloads/Soil_2022_GW_vuln_corr.xlsx")

#Exceedeance of bg leves soil 2022
Soil_2022_bg_excd <- full_join(Piv_1_Arica_Soil_2022_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(exceedences = ifelse(!is.na(value) & !is.na(`bg_level_(mg/kg)`), value >  `bg_level_(mg/kg)`, NA)) |> 
  group_by(analyte)|> 
  summarize(
    Exceedance_count = sum(exceedences),
    Exceedance_percent = sum(exceedences) / n() * 100,
    Sample_size = n(),
    .groups= "drop") |> print()

write_xlsx(Soil_2022_bg_excd,"/Users/illianasamorano/Downloads/Soil_2022_bg_excd_corr.xlsx")










