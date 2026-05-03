## OG Arica Dust manip for column chart
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggpubr)
library(tidyverse)

library(readxl)
arica_dust2022 <- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica_dust_data_Redo.xlsx", 
                             sheet = "FormattingforR")
print(arica_dust2022)

#Pivot longer analyte data
Piv_1_Arica_Dust <- 
  tibble(arica_dust2022) |> 
  pivot_longer(cols = beryllium:lead, names_to = "Analyte", values_to = "value") |> print()



# Lead-only dataset from pivoted data
column_chart_Pb_dust <- column_chart_Pb_dust |>
  arrange(type, value) |>   # group by type, then sort within group
  mutate(sample_index = row_number())

print(column_chart_Pb_dust)

# Plot
plt_Pb_d <- ggplot(column_chart_Pb_dust) +
  geom_col(aes(x = value,
               y = as.factor(sample_index),
               fill = as.factor(type)),
           width = 0.6) +
  
  labs(x = "Pb in Settled Dust (µg/ft²)", 
       y = "Sample Number",
       title = "Figure 7",
       fill = "Sampling Location",
       color = "Benchmark") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Indoor Floor Dust" = "pink",
    "Outdoor Porch Dust" = "coral2"
  )) +
  
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  
  geom_segment(
    aes(x = 5, xend = 5,
        y = factor(1), yend = factor(11),
        color = "US EPA DLAL Indoor"),
    linewidth = 0.5,
    linetype = "solid",
    inherit.aes = FALSE
  ) +
  
  scale_color_manual(values = c(
    "US EPA DLAL Indoor" = "blue"
  )) 

print(plt_Pb_d)