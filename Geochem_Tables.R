##Geochem

library(dbplyr)
library(tidyverse)
library(EnvStats)
library(writexl)

#Contamination Factor; Cmetal/Cbackground

#Import Data 2021
library(readxl)
Arica_Soil_Table_Redone_Soil_2021 <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                                sheet = "Soil 2021 data for analysis") |>print()

Piv_1_Arica_Soil_2021_redo <- 
  tibble(Arica_Soil_Table_Redone_Soil_2021) |> 
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value") |> print()

Bg_levels_soil_Arica <-tribble(
  ~"analyte",	~"bg_level_(mg/kg)",
  "copper",	25,
  "arsenic",	18.35,
  "chromium", 73.63, 
  "cadmium",	1.134,
  "nickel", 600,
  "zinc",	71,
  "lead",	11.89) |>print()

CF_2021 <- full_join(Piv_1_Arica_Soil_2021_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(CF = value / `bg_level_(mg/kg)` ) |> 
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

CF_2021_round <- CF_2021 |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
CF_2021_compress <- CF_2021_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

write_xlsx(CF_2021_compress,"/Users/illianasamorano/Downloads/CF_2021_compress_wchromo.xlsx")

#Import Data 2022
library(readxl)
Arica_Soil_2022_redone <- read_excel("~/Downloads/Arica Soil Table Redone.xlsx", 
                                     sheet = "Corrected 2022 Soil values") |> print()
Piv_1_Arica_Soil_2022_redo <- 
  tibble(Arica_Soil_2022_redone) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "analyte", values_to = "value") |> print()

CF_2022 <- full_join(Piv_1_Arica_Soil_2022_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(CF = value / `bg_level_(mg/kg)` ) |> 
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

CF_2022_round <- CF_2022 |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
CF_2022_compress <- CF_2022_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

write_xlsx(CF_2022_compress,"/Users/illianasamorano/Downloads/CF_2022_compress_wcromo.xlsx")

# Enrichment Factor
#lets go with aluminum as our reference metal for both; 
#https://link.springer.com/article/10.1007/s42452-020-03214-y#:~:text=The%20aluminum%20(Al)%20is%20chosen,of%20clay%20minerals%20%5B60%5D.
#Could not find Arica background aluminum concentration

#Geoaccumilation Index: Igeo=log2 (Cmetal/1.5*Cbackground)
#2021 Igeo
Igeo_2021 <- full_join(Piv_1_Arica_Soil_2021_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(Igeo = log2((value) / (1.5*`bg_level_(mg/kg)`))) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(Igeo),  
    SD = sd(Igeo),
    Median = median(Igeo),         
    Max = max(Igeo),               
    Min = min(Igeo),               
    Geomean = geoMean(Igeo),
    GeoSD = geoSD(Igeo),
    Sample_size = n(),
    .groups= "drop") |> print()

Igeo_2021_round <- Igeo_2021 |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Igeo_2021_compress <- Igeo_2021_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

write_xlsx(Igeo_2021_compress,"/Users/illianasamorano/Downloads/Igeo_2021_compressw.chro.xlsx")

#2022 Igeo
Igeo_2022 <- full_join(Piv_1_Arica_Soil_2022_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  mutate(Igeo = log2((value) / (1.5*`bg_level_(mg/kg)`))) |> 
  group_by(analyte)|> 
  summarize(
    Mean = mean(Igeo),  
    SD = sd(Igeo),
    Median = median(Igeo),         
    Max = max(Igeo),               
    Min = min(Igeo),               
    Geomean = geoMean(Igeo),
    GeoSD = geoSD(Igeo),
    Sample_size = n(),
    .groups= "drop") |> print()

Igeo_2022_round <- Igeo_2022 |> 
  mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), 
                ~ round(., 2))) |> print()

#Combine Mean(SD), Median(min-max), Geomean (geoSD) 
Igeo_2022_compress <- Igeo_2022_round |> 
  mutate("Mean(SD)" = paste(Mean, "(", SD, ")", sep = "")) |>
  mutate("Med(Min-Max)"= paste(Median, "(", Min,"-", Max, ")", sep = "")) |>
  mutate("Geomean(GeoSD)" = paste(Geomean, "(", GeoSD, ")", sep = "")) |> print()

write_xlsx(Igeo_2022_compress,"/Users/illianasamorano/Downloads/Igeo_2022_compresswchro.xlsx")


## Calculating PLI -> SUM of CF & Plotting
#PLI= nroot (CF1+CF2+CF3); n= # of pollutants, we have bg levels for 
#filter out NAs, 2022
CF_2022_for_PLI <- full_join(Piv_1_Arica_Soil_2022_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)`)) |>
  mutate(CF = value / `bg_level_(mg/kg)` ) |> view()

PLI_2022 <- CF_2022_for_PLI |> 
  group_by(sampleLong) |>  # Replace 'sample_id' with your actual sample identifier column
  summarize(PLI = (prod(CF))^(1/5), .groups = "drop") |>  # Take n root (n=5) of product of CFs
  print()

write_xlsx(PLI_2022,"/Users/illianasamorano/Downloads/PLI_2022_corrected.xlsx")

#PLIs 2021
CF_2021_for_PLI <- full_join(Piv_1_Arica_Soil_2021_redo, Bg_levels_soil_Arica, by = c("analyte" = "analyte"), relationship = "many-to-many") |>  
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)`)) |>
  mutate(CF = value / `bg_level_(mg/kg)` ) |> view()

PLI_2021 <- CF_2021_for_PLI |> 
  group_by(sampleLong) |>  # Replace 'sample_id' with your actual sample identifier column
  summarize(PLI = (prod(CF))^(1/5), .groups = "drop") |>  # Take n root (n=5) of product of CFs
  print()

write_xlsx(PLI_2021,"/Users/illianasamorano/Downloads/PLI_2021_corrected.xlsx")

#Plotting PLIs
#import consolidated PLI table; reimport corrected version

library(readxl)
Consolidated_PLI_for_plotting <- read_excel("~/Downloads/Consolidated PLI for plotting.xlsx", 
                                            sheet = "Corrected Formula")
View(Consolidated_PLI_for_plotting)

#Box plot for PLI
ggplot(Consolidated_PLI_for_plotting, aes(x = as.factor(Year), y = PLI)) +
  geom_violin(fill = "peachpuff2") + #how to make different colors for each yr
  geom_boxplot( # black color for box edges 
    width=0.25) +
  labs(x = "Project Year", 
       y = "PLI",
       title = "Pollution Load Indexes for Arica Soil Samples") +
  scale_y_continuous(breaks = seq(0, 20, by = 10), limits = c(0, 20)) +  # Set y-axis scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),  # Adjust x-axis text for readability
        plot.title = element_text(hjust = 0.5))

#Consider making a column chart

PLI_chart <- ggplot(Consolidated_PLI_for_plotting) +
  geom_col(aes(x = PLI, y = sampleLong, fill = as.factor(Year)),  # Map 'sampling_year' to 'fill'
           width = 0.5) +
  labs(
    x = "PLI", 
    y = "Sample ID",
    title = "PLI Column Chart",
    fill = "Project Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 20, by = 5)) +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "solid", size = 0.1, color = "blue") +
  geom_vline(xintercept = 3, linetype = "solid", size = 0.1, color = "orange") +
  geom_vline(xintercept = 6, linetype = "solid", size = 0.1, color = "red2") +
  scale_fill_manual(values = c("2021" = "peachpuff2", "2022" = "peachpuff4")) +
  scale_color_manual(values = c("Moderately Contaminated (1<PLI<3)" = "blue",
                                "Heavily Contaminated (3<PLI<6)" = "orange",
                                "Extremely Contaminated (6<PLI)" = "red2")) +
  geom_segment(aes(x = 1, xend = 1, y = -Inf, yend = Inf, color = ("Moderately Contaminated (1<PLI<3)")),
               size = 0.3) +   # Thinner line, no legend for size
  geom_segment(aes(x = 3, xend = 3, y = -Inf, yend = Inf, color = ("Heavily Contaminated (3<PLI<6)")),
               size = 0.3) +   # Thinner line, no legend for size
  geom_segment(aes(x = 6, xend = 6, y = -Inf, yend = Inf, color = ("Extremely Contaminated (6<PLI)")),
               size = 0.3) +   # Thinner line, no legend for size
  guides(color = guide_legend(title = "Benchmark PLIs"),
         override.aes = list(linetype = 1, size = 1, shape = NA))  +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5)
  )

print(PLI_chart)



