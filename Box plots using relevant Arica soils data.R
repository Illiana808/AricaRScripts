# Making Boxplots in R w/ Soil data

#Calling relevant packages
library(ggplot2)
library(multcompView)
library(dplyr)
library(tidyverse)


#Organizing Soil data by year & combining analyt & yr into one column for box plot creation

Piv_Arica_Soil_4_BoxP <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  group_by(Analyte, sampling_year)|>
  unite("Metal_by_Sample_Year", c("Analyte", "sampling_year"), sep = "(", remove = TRUE) |>
  mutate(Metal_by_Sample_Year = paste0(Metal_by_Sample_Year, ")"))  |> print()


# Now seperate out metal analytes that Zain's paper said had no statistically significant...
# difference between pXRF and ICPs values
# Recall --> OG data set contains "aluminum", "arsenic","barium", "beryllium", 
 # "cadmium", "chromium", "manganese","nickel", 
 # "zinc", "lead", "copper","vanadium"

#Zain's paper says only ones w/out stat sig difference
 #As, Ba, Ca, Cu, Mn, Pb, and Zn + Gift wants Cadmium even tho
# "With regards to the Arizona garden and yard samples, Co, Sb, Mo, Ag, Cd (cadium), Sn, and Sb concentrations were below the pXRF detection limits."

# Filtering Piv_Arica_Soil_4_plot for desired analyte rows

metals_to_include <- c("arsenic(2021)","arsenic(2022)",
                       "copper(2021)", "copper(2022)",
                       "manganese(2021)", "manganese(2022)",
                       "lead(2021)", "lead(2022)",
                       "zinc(2021)", "zinc(2022)",
                       "cadmium(2021)", "cadmium(2022)")

Piv_Arica_Soil_4_BoxP_filtered <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% metals_to_include) |> 
  print()

# Creating Box Plot for all metal analytes

#Creating custom colors for each analyte
custom_colors <- c("arsenic(2021)" = "darkolivegreen4", 
                   "arsenic(2022)" = "darkolivegreen4",
                   "copper(2021)"= "orange",
                   "copper(2022)"= "orange",
                   "lead(2021)" = "red", 
                   "lead(2022)" = "red",
                   "manganese(2021)" = "goldenrod4", 
                   "manganese(2022)" = "goldenrod4" ,  
                   "zinc(2021)" = "steelblue", 
                   "zinc(2022)" = "steelblue",
                   "cadmium(2021)" = "violetred4",
                   "cadmium(2022)" = "violetred4")


ggplot(Piv_Arica_Soil_4_BoxP_filtered, aes(x = Metal_by_Sample_Year, y = value, color = Metal_by_Sample_Year)) +
  geom_boxplot() +
  labs(x = "Metal Analyzed During Project Year", 
       y = "Concentration in Soil (mg/kg)",
       title = "Box Plot of Soil Metal Concentrations in and near Arica, Chile",
       color = "Metal(Gardenroots Project Year)") +
  scale_y_continuous(breaks = seq(0, 1500, by = 100)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Slants x-axis labels
  plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = custom_colors) |> print()

##NIX this visual; its not helpful to have global view of analytes since their bg & references values are so different
###Notes to self-> edit to remove underscores & additional customization

#Creating Individual box plots for better vis

# Box plot for Copper
copper_data <- c("copper(2021)","copper(2022)")

Piv_Arica_Soil_4_BoxP_filtered_Cu <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% copper_data) |> 
  mutate(Metal_by_Sample_Year = gsub("copper\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  print()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_Cu, aes(x = Metal_by_Sample_Year, y = value)) +
  geom_boxplot(fill = "orange", color = "darkorange4", outlier.shape = NA) +
  labs(x = "Project Year", 
       y = "Concentration of Copper in Soil (mg/kg)",
       title = "Figure 1") +
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits = c(0, 100)) + #that's how you adjust scale, nice 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0 , hjust = 0.5),
        plot.title= element_text(hjust = 0.5)) 


#Box Plot for Arsenic
arsenic_data <- c("arsenic(2021)","arsenic(2022)")

Piv_Arica_Soil_4_BoxP_filtered_As <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% arsenic_data) |> 
  mutate(Metal_by_Sample_Year = gsub("arsenic\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  print()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_As, aes(x = Metal_by_Sample_Year, y = value)) +
  geom_boxplot(fill = "darkolivegreen2", color = "darkolivegreen",  outlier.shape = NA) +
  labs(x = "Project Year", 
       y = "Concentration in Arsenic in Soil (mg/kg)",
       title = "Figure 2") + 
  scale_y_continuous(breaks = seq(0, 50, by = 5), limits = c(0, 50)) +  #that's how you adjust scale, nice
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title= element_text(hjust = 0.5))  

#Box Plot for Manganese
Manganese_data <- c("manganese(2021)","manganese(2022)")

Piv_Arica_Soil_4_BoxP_filtered_Mn <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% Manganese_data) |> 
  mutate(Metal_by_Sample_Year = gsub("manganese\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  print()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_Mn, aes(x = Metal_by_Sample_Year, y = value)) +
  geom_boxplot(fill = "gold", color = "goldenrod4", outlier.shape = NA) +
  labs(x = "Project Year", 
       y = "Concentration of Manganese in Soil (mg/kg)",
       title = "Figure 3") + 
  scale_y_continuous(breaks = seq(0, 650, by = 50), limits = c(0, 650)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title= element_text(hjust = 0.5)) 

#Box Plot for Lead
lead_data <- c("lead(2021)","lead(2022)")

Piv_Arica_Soil_4_BoxP_filtered_Pb <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% lead_data) |> 
  mutate(Metal_by_Sample_Year = gsub("lead\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  print()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_Pb, aes(x = Metal_by_Sample_Year, y = value)) +
  geom_boxplot(fill = "tomato", color = "darkred", outlier.shape = NA) +
  labs(x = "Project Year", 
       y = "Concentration of Lead in Soil (mg/kg)",
       title = "Figure 4") + 
  scale_y_continuous(breaks = seq(0, 120, by = 10), limits = c(0,120)) + #that's how you adjust scale, nice
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title= element_text(hjust = 0.5))


#Box Plot for Zinc
zinc_data <- c("zinc(2021)","zinc(2022)")

Piv_Arica_Soil_4_BoxP_filtered_Zn <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% zinc_data) |> 
  mutate(Metal_by_Sample_Year = gsub("zinc\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  print()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_Zn, aes(x = Metal_by_Sample_Year, y = value)) +
  geom_boxplot(fill = "lightblue", color = "steelblue4", outlier.shape = NA ) +
  labs(x = "Project Year", 
       y = "Concentration of Zinc in Soil (mg/kg)",
       title = "Figure 5") + 
  scale_y_continuous(breaks = seq(0, 240, by = 20), limits=c(0,240)) + #that's how you adjust scale, nice
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title= element_text(hjust = 0.5))

#Box Plot for Cadmium 
cadmium_data <- c("cadmium(2021)","cadmium(2022)")

Piv_Arica_Soil_4_BoxP_filtered_Cd <- Piv_Arica_Soil_4_BoxP |> 
  filter(Metal_by_Sample_Year %in% cadmium_data) |>
  mutate(Metal_by_Sample_Year = gsub("cadmium\\((\\d{4})\\)", "\\1", Metal_by_Sample_Year)) |>
  mutate (log = log(value))|>
  view()

ggplot(Piv_Arica_Soil_4_BoxP_filtered_Cd, aes(x = Metal_by_Sample_Year, y = log)) +
  geom_boxplot(fill = "violetred1", color = "violetred4", outlier.shape = NA ) +
  labs(x = "Project Year", 
       y = "Log Concentration of Cadmium in Soil",
       title = "Figure 6") + 
  scale_y_continuous(breaks = seq(-2, 3, by = 0.5), limits = c(-2,3)) + #that's how you adjust scale, nice
  theme_minimal()  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title= element_text(hjust = 0.5))


