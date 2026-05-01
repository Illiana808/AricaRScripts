
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggpubr)
library(tidyverse)


# Pivoting data for column chart
Piv_Arica_Soil_4_BoxP <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  group_by(Analyte, sampling_year) |> print()

#copper column chart

column_chart_Cu <-  tibble(column_chart) |> 
  filter(Analyte == "copper")|> 
  print()

plt_Cu <- ggplot(column_chart_Cu) +
  geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
             width = 0.5) +
    labs(
      x = "Concentration of Copper in Soil (mg/kg)", 
      y = "Sample ID",
      title = "Figure 1.2",
      fill = "Project Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(0, 600, by = 40)) +
    theme_minimal() +
    geom_vline(xintercept = 25, linetype = "solid", size = 0.3, color = "blue")  +
    geom_vline(xintercept = 39, linetype = "solid", size = 0.3, color = "red")   +
  scale_fill_manual(values = c("2021" = "orange", "2022" = "darkorange4")) +
  scale_color_manual(values = c("Background Concentration" = "blue", 
                                "US EPA Screening Level" = "red")) +
  geom_segment(aes(x = 25, xend = 25, y = -Inf, yend = Inf, color = "Background Concentration"), 
               size = 0.3) + 
  geom_segment(aes(x = 39, xend = 39, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
               size = 0.3) +
  guides(color = guide_legend(title = "Benchmark Concentrations"),
         override.aes = list(linetype = 1, size = 1, shape = NA))  # Change legend items to lines
print(plt_Cu)

  
#Arsenic column chart
  column_chart <- 
   tibble(Arica_Soil_data_basic_stats_v_2) |> 
     pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
     group_by(Analyte, sampling_year) |> 
     print()
   
   column_chart_As <-  tibble(column_chart) |> 
     filter(Analyte == "arsenic")|> 
     print()
   
   plt_As <- ggplot(column_chart_As) +
     geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
              width = 0.5) +
     labs(
       x = "Concentration of Arsenic in Soil (mg/kg)", 
       y = "Sample ID",
       title = "Figure 2.2",
       fill = "Project Year") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = seq(0, 440, by = 40)) +
     theme_minimal() +
     geom_vline(xintercept = 18.35, linetype = "solid", size = 0.3, color = "blue")  +
     geom_vline(xintercept = 0.68, linetype = "solid", size = 0.3, color = "red")   +
     scale_fill_manual(values = c("2021" = "lightgreen", "2022" = "darkgreen")) +
     scale_color_manual(values = c("Background Concentration" = "blue", 
                                   "US EPA Screening Level" = "red")) +
     geom_segment(aes(x = 18.35, xend = 18.35, y = -Inf, yend = Inf, color = "Background Concentration"), 
                  size = 0.3) + 
     geom_segment(aes(x = 0.68, xend = 0.68, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
                  size = 0.3) +
     guides(color = guide_legend(title = "Benchmark Concentrations"),
            override.aes = list(linetype = 1, size = 1, shape = NA))  
   print(plt_As)
####

#Manganese column chart
   column_chart_Mn <-  tibble(column_chart) |> 
     filter(Analyte == "manganese")|> 
     print()
   
   plt_Mn <- ggplot(column_chart_Mn) +
     geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
              width = 0.5) +
     labs(
       x = "Concentration of Manganese in Soil (mg/kg)", 
       y = "Sample ID",
       title = "Figure 3.2",
       fill = "Project Year") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = seq(0, 1400, by = 50)) +
     theme_minimal()   +
      geom_vline(xintercept = 180, linetype = "solid", size = 0.3, color = "red")   +
     scale_fill_manual(values = c("2021" = "gold", "2022" = "goldenrod4")) +
     scale_color_manual(values = c("US EPA Screening Level" = "red")) + # could not find bg conc
     geom_segment(aes(x = 180, xend = 180, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
                  size = 0.3) +
     guides(color = guide_legend(title = "Benchmark Concentrations"),
            override.aes = list(linetype = 1, size = 1, shape = NA)) 
   print(plt_Mn)

####
   
 #lead column chart
   column_chart_Pb <-  tibble(column_chart) |> 
     filter(Analyte == "lead")|> 
     print()
   
   plt_Pb <- ggplot(column_chart_Pb) +
     geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
              width = 0.5) +
     labs(
       x = "Concentration of Lead in Soil (mg/kg)", 
       y = "Sample ID",
       title = "Figure 4.2",
       fill = "Project Year") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = seq(0, 1400, by = 100)) +
     theme_minimal()   +
     geom_vline(xintercept = 11.89, linetype = "solid", size = 0.3, color = "blue")  +
     geom_vline(xintercept = 100, linetype = "solid", size = 0.3, color = "red2")   +
     scale_fill_manual(values = c("2021" = "pink", "2022" = "coral")) +
     scale_color_manual(values = c("Background Concentration" = "blue", 
                                   "US EPA Screening Level" = "red2")) +
     geom_segment(aes(x = 11.89, xend = 11.89, y = -Inf, yend = Inf, color = "Background Concentration"), 
                  size = 0.3) + 
     geom_segment(aes(x = 100, xend = 100, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
                  size = 0.3) +
     guides(color = guide_legend(title = "Benchmark Concentrations"),
            override.aes = list(linetype = 1, size = 1, shape = NA))  +
     theme(
       axis.text.x = element_text(size = 8, angle = 45)  )
   print(plt_Pb)

   
#cadmium column chart
   column_chart_Cd <-  tibble(column_chart) |> 
     filter(Analyte == "cadmium")|> 
     print()
   
   plt_Cd <- ggplot(column_chart_Cd) +
     geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
              width = 0.5) +
     labs(
       x = "Concentration of Cadmium in Soil (mg/kg)", 
       y = "Sample ID",
       title = "Figure 6.2",
       fill = "Project Year") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = seq(0, 30, by = 5)) +
     theme_minimal()   +
     geom_vline(xintercept = 1.134, linetype = "solid", size = 0.3, color = "blue")  +
     geom_vline(xintercept = 0.71, linetype = "solid", size = 0.3, color = "red2")   +
     scale_fill_manual(values = c("2021" = "violetred1", "2022" = "violetred4")) +
     scale_color_manual(values = c("Background Concentration" = "blue", 
                                   "US EPA Screening Level" = "red2")) +
     geom_segment(aes(x = 1.134, xend = 1.134, y = -Inf, yend = Inf, color = "Background Concentration"), 
                  size = 0.3) + 
     geom_segment(aes(x = 0.71, xend = 0.71, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
                  size = 0.3) +
     guides(color = guide_legend(title = "Benchmark Concentrations"),
            override.aes = list(linetype = 1, size = 1, shape = NA))  +
     theme(
       axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5)  )
   print(plt_Cd)

 #zinc column chart
   column_chart_Zn <-  tibble(column_chart) |> 
     filter(Analyte == "zinc")|> 
     print()
   
   plt_Zn <- ggplot(column_chart_Zn) +
     geom_col(aes(x = value, y = sampleLong, fill = as.factor(sampling_year)),  # Map 'sampling_year' to 'fill'
              width = 0.5) +
     labs(
       x = "Concentration of Zinc in Soil (mg/kg)", 
       y = "Sample ID",
       title = "Figure 5.2",
       fill = "Project Year") +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = seq(0, 2300, by = 100)) +
     theme_minimal()   +
     geom_vline(xintercept = 71, linetype = "solid", size = 0.3, color = "blue")  +
     geom_vline(xintercept = 2300, linetype = "solid", size = 0.3, color = "red2")   +
     scale_fill_manual(values = c("2021" = "lightblue1", "2022" = "steelblue4")) +
     scale_color_manual(values = c("Background Concentration" = "blue", 
                                   "US EPA Screening Level" = "red2")) +
     geom_segment(aes(x = 71, xend = 71, y = -Inf, yend = Inf, color = "Background Concentration"), 
                  size = 0.3) + 
     geom_segment(aes(x = 2300, xend = 2300, y = -Inf, yend = Inf, color = "US EPA Screening Level"), 
                  size = 0.3) +
     guides(color = guide_legend(title = "Benchmark Concentrations"),
            override.aes = list(linetype = 1, size = 1, shape = NA))  +
     theme(
       axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5)  )
   print(plt_Zn)
   
