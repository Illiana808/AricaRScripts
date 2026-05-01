## OG Arica Dust manip for column chart
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggpubr)
library(tidyverse)

column_chart_dust <- 
  tibble(Arica_dust_data_basic_stats) |> 
  pivot_longer(cols = aluminum:copper, names_to = "Analyte", values_to = "value") |>
  group_by(Analyte, type) |> 
  print()

#lead dust column chart
column_chart_Pb_dust <-  tibble(column_chart_dust) |> 
  filter(Analyte == "lead")|> 
  print()

plt_Pb_d <- ggplot(column_chart_Pb_dust) +
  geom_col(aes(x = value, y = sampleLong, fill = as.factor(type)),  # Map 'sampling_year' to 'fill'
           width = 0.5) +
  labs(
    x = "Concentration of Lead in Dust (mu*g/ft^2)", 
    y = "Sample ID",
    title = "Figure 7",
    fill = "Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  theme_minimal()   +
  scale_fill_manual(values = c("Indoor Floor Dust" = "pink", "Outdoor Porch Dust" = "coral2")) +
  scale_color_manual(values = c("US EPA DLRL Indoor" = "red", 
                                "US EPA DLAL Indoor" = "blue",
                                "US EPA DLAL Outdoor" = "orange")) +
  geom_segment(aes(x = 0, xend = 0, y = "NSARICA2022-PI01" , yend = "NSARICA2022-PI11" , color = "US EPA DLRL Indoor"), 
               size = 0.3) + 
  geom_segment(aes(x = 5, xend = 5,  y = "NSARICA2022-PI01" , yend = "NSARICA2022-PI11" , color = "US EPA DLAL Indoor"), 
               size = 0.3) + 
  geom_segment(aes(x = 100, xend = 100,  y = "NSARICA2022-PE01" , yend = "NSARICA2022-PE11", color = "US EPA DLAL Outdoor"), 
               size = 0.3) +
  guides(color = guide_legend(title = "Benchmark Concentrations"),
         override.aes = list(linetype = 1, size = 1, shape = NA))  +
  theme(
    axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5)  )
print(plt_Pb_d)

