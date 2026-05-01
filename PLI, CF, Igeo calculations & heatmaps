# ============================================================
# Geochem Analysis: CF, Igeo, PLI, and Heatmaps
# Arica, Chile Soil Data — 2021 & 2022
# ============================================================

library(dbplyr)
library(tidyverse)
library(EnvStats)
library(writexl)
library(readxl)
library (ggplot2)

# ============================================================
# IMPORT DATA
# ============================================================

Arica_Soil_Table_Redone_2021 <- read_excel(
  "~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx",
  sheet = "Soil 2021 data for analysis"
)

Piv_1_Arica_Soil_2021_redo <-
  tibble(Arica_Soil_Table_Redone_2021) |>
  pivot_longer(cols = beryllium:uranium, names_to = "analyte", values_to = "value")

Arica_Soil_2022_redone<- read_excel("~/Library/CloudStorage/Box-Box/Illiana Samorano/Arica_Chile/Data_soil_dust/Arica Data Workspace/Arica Soil Table Redone.xlsx", 
                                    sheet = "Corrected 2022 Soil values")
Piv_1_Arica_Soil_2022_redo <- 
  tibble(Arica_Soil_2022_redone) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "analyte", values_to = "value") |> print()

# ============================================================
# BACKGROUND LEVELS
# ============================================================

Bg_Levels_CENMA_2013 <- tribble(
  ~analyte,       ~`bg_level_(mg/kg)_C`,
  "arsenic",       18.35,
  "chromium",      73.63,
  "cadmium",        1.134,
  "lead",          11.89
)

Bg_Levels_Tume_2018 <- tribble(
  ~analyte,    ~`bg_level_(mg/kg)_T`,
  "arsenic",    17.4,
  "barium",     23.3,
  "chromium",   13.6,
  "copper",     27.4,
  "lead",      313.0,
  "nickel",      8.3,
  "vanadium",  101.0,
  "zinc",      235.0
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

summarize_and_format_CF <- function(df, bg_col) {
  df |>
    mutate(CF = value / .data[[bg_col]]) |>
    group_by(analyte) |>
    summarize(
      Mean      = mean(CF),
      SD        = sd(CF),
      Median    = median(CF),
      Max       = max(CF),
      Min       = min(CF),
      Geomean   = geoMean(CF),
      GeoSD     = geoSD(CF),
      Sample_size = n(),
      .groups = "drop"
    ) |>
    mutate(across(c(Mean, SD, Median, Min, Max, Geomean, GeoSD), ~ round(., 2))) |>
    mutate(
      `Mean(SD)`        = paste0(Mean, "(", SD, ")"),
      `Med(Min-Max)`    = paste0(Median, "(", Min, "-", Max, ")"),
      `Geomean(GeoSD)`  = paste0(Geomean, "(", GeoSD, ")")
    ) |>
    select(analyte, `Mean(SD)`, `Med(Min-Max)`, `Geomean(GeoSD)`)
}

summarize_and_format_Igeo <- function(df, bg_col) {
  df |>
    mutate(Igeo = log2(value / (1.5 * .data[[bg_col]]))) |>
    group_by(analyte) |>
    summarize(
      Mean    = mean(Igeo),
      SD      = sd(Igeo),
      Median  = median(Igeo),
      Max     = max(Igeo),
      Min     = min(Igeo),
      Sample_size = n(),
      .groups = "drop"
    ) |>
    mutate(across(c(Mean, SD, Median, Min, Max), ~ round(., 2))) |>
    mutate(
      `Mean(SD)`     = paste0(Mean, "(", SD, ")"),
      `Med(Min-Max)` = paste0(Median, "(", Min, "-", Max, ")")
    ) |>
    select(analyte, `Mean(SD)`, `Med(Min-Max)`)
}

# ============================================================
# 2021 — CF & IGEO (CENMA)
# ============================================================

joined_2021_C <- full_join(
  Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013,
  by = "analyte", relationship = "many-to-many"
)

CF_2021_comb_C   <- summarize_and_format_CF(joined_2021_C,   "bg_level_(mg/kg)_C")
Igeo_2021_comb_C <- summarize_and_format_Igeo(joined_2021_C, "bg_level_(mg/kg)_C")

write_xlsx(CF_2021_comb_C,   "~/Downloads/CF_2021_CENMA.xlsx")
write_xlsx(Igeo_2021_comb_C, "~/Downloads/Igeo_2021_CENMA.xlsx")

# ============================================================
# 2021 — CF & IGEO (TUME)
# ============================================================

joined_2021_T <- full_join(
  Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018,
  by = "analyte", relationship = "many-to-many"
)

CF_2021_comb_T   <- summarize_and_format_CF(joined_2021_T,   "bg_level_(mg/kg)_T")
Igeo_2021_comb_T <- summarize_and_format_Igeo(joined_2021_T, "bg_level_(mg/kg)_T")

write_xlsx(CF_2021_comb_T,   "~/Downloads/CF_2021_Tume.xlsx")
write_xlsx(Igeo_2021_comb_T, "~/Downloads/Igeo_2021_Tume.xlsx")

# ============================================================
# 2022 — CF & IGEO (CENMA)
# ============================================================

joined_2022_C <- full_join(
  Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013,
  by = "analyte", relationship = "many-to-many"
)

CF_2022_comb_C   <- summarize_and_format_CF(joined_2022_C,   "bg_level_(mg/kg)_C")
Igeo_2022_comb_C <- summarize_and_format_Igeo(joined_2022_C, "bg_level_(mg/kg)_C")

write_xlsx(CF_2022_comb_C,   "~/Downloads/CF_2022_CENMA.xlsx")
write_xlsx(Igeo_2022_comb_C, "~/Downloads/Igeo_2022_CENMA.xlsx")

# ============================================================
# 2022 — CF & IGEO (TUME)
# ============================================================

joined_2022_T <- full_join(
  Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018,
  by = "analyte", relationship = "many-to-many"
)

CF_2022_comb_T   <- summarize_and_format_CF(joined_2022_T,   "bg_level_(mg/kg)_T")
Igeo_2022_comb_T <- summarize_and_format_Igeo(joined_2022_T, "bg_level_(mg/kg)_T")

write_xlsx(CF_2022_comb_T,   "~/Downloads/CF_2022_Tume.xlsx")
write_xlsx(Igeo_2022_comb_T, "~/Downloads/Igeo_2022_Tume.xlsx")

# ============================================================
# PLI — CENMA (n=4)
# ============================================================

make_PLI <- function(soil_piv, bg_df, bg_col, n_metals, id_col) {
  full_join(soil_piv, bg_df, by = "analyte", relationship = "many-to-many") |>
    filter(!is.na(value) & !is.na(.data[[bg_col]])) |>
    mutate(CF = value / .data[[bg_col]]) |>
    group_by(across(all_of(id_col))) |>
    summarize(PLI = prod(CF)^(1 / n_metals), .groups = "drop")
}

PLI_CENMA_2021 <- make_PLI(Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", 4, "sampleLong")
PLI_CENMA_2022 <- make_PLI(Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", 4, "sampleLong")
PLI_Tume_2021  <- make_PLI(Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018,  "bg_level_(mg/kg)_T", 7, "sampleLong")
PLI_Tume_2022  <- make_PLI(Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018,  "bg_level_(mg/kg)_T", 7, "sampleLong")

write_xlsx(PLI_CENMA_2021, "~/Downloads/PLI_2021_CENMA.xlsx")
write_xlsx(PLI_CENMA_2022, "~/Downloads/PLI_CENMA_2022.xlsx")
write_xlsx(PLI_Tume_2021,  "~/Downloads/PLI_2021_Tume.xlsx")
write_xlsx(PLI_Tume_2022,  "~/Downloads/PLI_Tume_2022.xlsx")

# ============================================================
# IMPORT CONSOLIDATED PLI TABLES (with Sample_Number column)
# ============================================================

Consolidated_CENMA_PLI <- read_excel(
  "~/Downloads/Consolidated PLIs for both bg level sets.xlsx",
  sheet = "CENMA"
)

Consolidated_Tume_PLI <- read_excel(
  "~/Downloads/Consolidated PLIs for both bg level sets.xlsx",
  sheet = "Tume"
)

# ============================================================
# PLI VIOLIN PLOTS
# ============================================================

ggplot(Consolidated_CENMA_PLI, aes(x = as.factor(Year), y = PLI)) +
  geom_violin(fill = "orange") +
  geom_boxplot(width = 0.15) +
  labs(x = "Project Year", y = "CENMA 2013 PLI") +
  scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5)) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5),
    axis.text    = element_text(size = 10),
    axis.title   = element_text(size = 15),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggplot(Consolidated_Tume_PLI, aes(x = as.factor(Year), y = PLI)) +
  geom_violin(fill = "orchid") +
  geom_boxplot(width = 0.15) +
  labs(x = "Project Year", y = "Tume et al. 2018 PLI") +
  scale_y_continuous(breaks = seq(0, 5, by = 1), limits = c(0, 5)) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5),
    axis.text    = element_text(size = 10),
    axis.title   = element_text(size = 15),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# ============================================================
# PLI COLUMN CHARTS (CENMA & TUME)
# Shared theme + benchmark line helper
# ============================================================

pli_benchmark_colors <- c(
  "Moderately Contaminated (1<PLI<3)" = "blue",
  "Heavily Contaminated (3<PLI<6)"    = "orange",
  "Extremely Contaminated (6<PLI)"    = "red2"
)

add_pli_benchmarks <- function(p) {
  p +
    geom_segment(aes(x = 1, xend = 1, y = -Inf, yend = Inf,
                     color = "Moderately Contaminated (1<PLI<3)"), linewidth = 0.3) +
    geom_segment(aes(x = 3, xend = 3, y = -Inf, yend = Inf,
                     color = "Heavily Contaminated (3<PLI<6)"),    linewidth = 0.3) +
    geom_segment(aes(x = 6, xend = 6, y = -Inf, yend = Inf,
                     color = "Extremely Contaminated (6<PLI)"),    linewidth = 0.3) +
    scale_color_manual(values = pli_benchmark_colors) +
    guides(
      color = guide_legend(title = "Benchmark PLIs",
                           override.aes = list(linetype = 1, linewidth = 1, shape = NA)),
      fill  = guide_legend(title = "Project Year")
    )
}

pli_theme <- theme(
  axis.text.x      = element_text(size = 7, angle = 45, hjust = 0.5),
  axis.text        = element_text(size = 10),
  axis.title       = element_text(size = 15),
  panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
  legend.position  = "bottom",
  legend.box       = "vertical",
  legend.title.align = 0.5,
  panel.grid       = element_blank(),
  legend.text      = element_text(size = 10),
  legend.background = element_rect(linetype = 1, linewidth = 0.25, color = "black")
)

# CENMA column chart
PLI_chart_CENMA <- ggplot(Consolidated_CENMA_PLI_sort) +
  geom_col(
    aes(x = PLI,
        y = factor(Sample_Number, levels = sort(unique(Sample_Number))),
        fill = as.factor(Year)),
    width = 0.5,
    position = position_dodge2(preserve = "single")   # <-- changed
  ) +
  labs(x = "CENMA 2013 PLI", y = "Sample", fill = "Project Year") +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_fill_manual(values = c("2021" = "orange", "2022" = "orange4")) +
  theme_minimal() +
  pli_theme

PLI_chart_CENMA <- add_pli_benchmarks(PLI_chart_CENMA)
plot(PLI_chart_CENMA)

# TUME column chart
PLI_chart_Tume <- ggplot(Consolidated_Tume_PLI_sort) +
  geom_col(
    aes(x = PLI,
        y = factor(Sample_Number, levels = sort(unique(Sample_Number))),
        fill = as.factor(Year)),
    width = 0.5        # <-- removed the extra ) here
  ) +
  labs(x = "Tume et al. 2018 PLI", y = "Sample", fill = "Project Year") +
  scale_x_continuous(breaks = seq(0, 20, by = 1)) +
  scale_fill_manual(values = c("2021" = "orchid1", "2022" = "purple4")) +
  theme_minimal() +
  pli_theme

PLI_chart_Tume <- add_pli_benchmarks(PLI_chart_Tume)
plot(PLI_chart_Tume)

### quick stats for presentation







# ============================================================
# HEATMAPS — CF (Sample ID x Analyte)
# Raw per-sample CF values (not summarized means)
# ============================================================

make_CF_heatmap <- function(soil_piv, bg_df, bg_col, year_label, bg_label, fill_colors) {
  soil_piv |>
    full_join(bg_df, by = "analyte", relationship = "many-to-many") |>
    filter(!is.na(value) & !is.na(.data[[bg_col]])) |>
    mutate(CF = value / .data[[bg_col]]) |>
    ggplot(aes(x = sampleLong, y = analyte, fill = CF)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low      = fill_colors[1],
      mid      = fill_colors[2],
      high     = fill_colors[3],
      midpoint = 1,
      name     = "CF"
    ) +
    labs(
      title = paste("Contamination Factor —", bg_label, year_label),
      x     = "Sample ID",
      y     = "Analyte"
    ) +
    theme_minimal() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 10),
      axis.title       = element_text(size = 13),
      plot.title       = element_text(hjust = 0.5, size = 14),
      panel.grid       = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.title     = element_text(size = 11),
      legend.text      = element_text(size = 9)
    )
}

# CF heatmaps — CENMA (orange palette)
plot(make_CF_heatmap(Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013,
                      "bg_level_(mg/kg)_C", "2021", "CENMA 2013",
                      c("white", "orange", "darkred")))

plot(make_CF_heatmap(Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013,
                      "bg_level_(mg/kg)_C", "2022", "CENMA 2013",
                      c("white", "orange", "darkred")))

#Combine years 
# Combine 2021 and 2022 data with a Year label
# Create a Sample_Number key from 2021 data
sample_key_2021 <- Piv_1_Arica_Soil_2021_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Create a Sample_Number key from 2022 data
sample_key_2022 <- Piv_1_Arica_Soil_2022_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Combine with Sample_Number mapped in
CF_heatmap_CENMA_combined <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> 
    left_join(sample_key_2021, by = "sampleLong") |> 
    mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> 
    left_join(sample_key_2022, by = "sampleLong") |> 
    mutate(Year = "2022")
) |>
  full_join(Bg_Levels_CENMA_2013, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_C`)) |>
  mutate(CF = value / `bg_level_(mg/kg)_C`)

# Plot with Sample_Number on x-axis
#CF_heatmap_CENMA_plot <- ggplot(CF_heatmap_CENMA_combined,
                                aes(x = factor(Sample_Number), y = analyte, fill = CF)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low      = "white",
    mid      = "orange",
    high     = "darkred",
    midpoint = 1,
    name     = "CF"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "CENMA Contamination Factor Heat Map",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y    = element_text(size = 10),
    axis.title     = element_text(size = 13),
    plot.title     = element_text(hjust = 0.5, size = 14),
    panel.grid     = element_blank(),
    panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text     = element_text(size = 12, face = "bold"),
    legend.title   = element_text(size = 11),
    legend.text    = element_text(size = 9)
  )

#Use this one for better gradient at low values
CF_heatmap_CENMA_plot <- ggplot(CF_heatmap_CENMA_combined,
                               aes(x = factor(Sample_Number), y = analyte, fill = CF)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 15, 30, 45, 60, 75, 80, 100, 120)),  # stretched near 0-1 for visibility
    limits  = c(0, 30),
    oob     = scales::squish,
    name    = "CF"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "CENMA 2013 Contamination Factor Heat Map",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text   = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

plot(CF_heatmap_CENMA_plot)

# CF heatmaps — Tume (purple palette)
plot(make_CF_heatmap(Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018,
                      "bg_level_(mg/kg)_T", "2021", "Tume et al. 2018",
                      c("white", "orchid1", "purple4")))

plot(make_CF_heatmap(Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018,
                      "bg_level_(mg/kg)_T", "2022", "Tume et al. 2018",
                      c("white", "orchid1", "purple4")))

#Combine Years for Tume 
#Combine years 
# Combine 2021 and 2022 data with a Year label
# Create a Sample_Number key from 2021 data
sample_key_2021 <- Piv_1_Arica_Soil_2021_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Create a Sample_Number key from 2022 data
sample_key_2022 <- Piv_1_Arica_Soil_2022_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Combine with Sample_Number mapped in
CF_heatmap_Tume_combined <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> 
    left_join(sample_key_2021, by = "sampleLong") |> 
    mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> 
    left_join(sample_key_2022, by = "sampleLong") |> 
    mutate(Year = "2022")
) |>
  full_join(Bg_Levels_Tume_2018, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_T`)) |>
  mutate(CF = value / `bg_level_(mg/kg)_T`)

# Plot with Sample_Number on x-axis
#CF_heatmap_Tume_plot <- ggplot(CF_heatmap_Tume_combined,
                                aes(x = factor(Sample_Number), y = analyte, fill = CF)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low      = "white",
    mid      = "orchid1",
    high     = "purple4",
    midpoint = 1,
    name     = "CF"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "Tume Contamination Factor Heat Map",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y    = element_text(size = 10),
    axis.title     = element_text(size = 13),
    plot.title     = element_text(hjust = 0.5, size = 14),
    panel.grid     = element_blank(),
    panel.border   = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text     = element_text(size = 12, face = "bold"),
    legend.title   = element_text(size = 11),
    legend.text    = element_text(size = 9)
  )

##do this one for better gradient
CF_heatmap_Tume_plot <- ggplot(CF_heatmap_Tume_combined,
                               aes(x = factor(Sample_Number), y = analyte, fill = CF)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 15, 30)),  # stretched near 0-1 for visibility
    limits  = c(0, 30),
    oob     = scales::squish,
    name    = "CF"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "Tume et al. 2018 Contamination Factor Heat Map",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text   = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

plot(CF_heatmap_Tume_plot)

# ============================================================
# HEATMAPS — IGEO (Sample ID x Analyte)
# Midpoint = 0 (uncontaminated threshold)
# ============================================================

make_Igeo_heatmap <- function(soil_piv, bg_df, bg_col, year_label, bg_label, fill_colors) {
  soil_piv |>
    full_join(bg_df, by = "analyte", relationship = "many-to-many") |>
    filter(!is.na(value) & !is.na(.data[[bg_col]])) |>
    mutate(Igeo = log2(value / (1.5 * .data[[bg_col]]))) |>
    ggplot(aes(x = sampleLong, y = analyte, fill = Igeo)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low      = fill_colors[1],
      mid      = fill_colors[2],
      high     = fill_colors[3],
      midpoint = 0,
      name     = "Igeo"
    ) +
    labs(
      title = paste("Geoaccumulation Index (Igeo) —", bg_label, year_label),
      x     = "Sample ID",
      y     = "Analyte"
    ) +
    theme_minimal() +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 10),
      axis.title       = element_text(size = 13),
      plot.title       = element_text(hjust = 0.5, size = 14),
      panel.grid       = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.title     = element_text(size = 11),
      legend.text      = element_text(size = 9)
    )
}

# Igeo heatmaps — CENMA
plot(make_Igeo_heatmap(Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013,
                        "bg_level_(mg/kg)_C", "2021", "CENMA 2013",
                        c("steelblue", "white", "darkred")))

plot(make_Igeo_heatmap(Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013,
                        "bg_level_(mg/kg)_C", "2022", "CENMA 2013",
                        c("steelblue", "white", "darkred")))

#plot both yrs
# Combine 2021 and 2022 data with a Year label
# Create a Sample_Number key from 2021 data
Igeo_heatmap_CENMA_combined <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> 
    left_join(sample_key_2021, by = "sampleLong") |> 
    mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> 
    left_join(sample_key_2022, by = "sampleLong") |> 
    mutate(Year = "2022")
) |>
  full_join(Bg_Levels_CENMA_2013, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_C`)) |>
  mutate(Igeo = log2(value / (1.5 * `bg_level_(mg/kg)_C`)))  # <-- fixed

Igeo_heatmap_CENMA_plot <- ggplot(Igeo_heatmap_CENMA_combined,
                                  aes(x = factor(Sample_Number), y = analyte, fill = Igeo)) +  # <-- Igeo
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("steelblue", "deepskyblue", "white", "orange", "darkred"),
    values  = scales::rescale(c(-4, -1, 0, 2, 6)),  # anchor colors to meaningful Igeo thresholds
    limits  = c(-4, 6),
    oob     = scales::squish,
    name    = "Igeo"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "Geoaccumulation Index (Igeo) — CENMA 2013",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text   = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

plot(Igeo_heatmap_CENMA_plot)


# Igeo heatmaps — Tume
print(make_Igeo_heatmap(Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018,
                        "bg_level_(mg/kg)_T", "2021", "Tume et al. 2018",
                        c("steelblue", "white", "darkred")))

print(make_Igeo_heatmap(Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018,
                        "bg_level_(mg/kg)_T", "2022", "Tume et al. 2018",
                        c("steelblue", "white", "darkred")))

# Combine 2021 and 2022 data with a Year label
# Create a Sample_Number key from 2021 data
Igeo_heatmap_Tume_combined <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> 
    left_join(sample_key_2021, by = "sampleLong") |> 
    mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> 
    left_join(sample_key_2022, by = "sampleLong") |> 
    mutate(Year = "2022")
) |>
  full_join(Bg_Levels_Tume_2018, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_T`)) |>
  mutate(Igeo = log2(value / (1.5 * `bg_level_(mg/kg)_T`)))  # <-- fixed

Igeo_heatmap_Tume_plot <- ggplot(Igeo_heatmap_Tume_combined,
                                  aes(x = factor(Sample_Number), y = analyte, fill = Igeo)) +  # <-- Igeo
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("steelblue", "deepskyblue", "white", "orange", "darkred"),
    values  = scales::rescale(c(-4, -1, 0, 2, 6)),  # anchor colors to meaningful Igeo thresholds
    limits  = c(-4, 6),
    oob     = scales::squish,
    name    = "Igeo"
  ) +
  facet_wrap(~ Year, scales = "free_x", ncol = 2) +
  labs(
    title = "Geoaccumulation Index (Igeo) — Tume 2018",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 9),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text   = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

plot(Igeo_heatmap_Tume_plot)

#####Getting % of contaminated

# PLI CATEGORY SUMMARY — % of samples per year per category
# ============================================================

categorize_PLI <- function(df, year_label) {
  df |>
    mutate(
      Year = year_label,
      PLI_Category = case_when(
        PLI < 1              ~ "Uncontaminated (PLI<1)",
        PLI >= 1 & PLI < 3  ~ "Moderately Contaminated (1<PLI<3)",
        PLI >= 3 & PLI < 6  ~ "Heavily Contaminated (3<PLI<6)",
        PLI >= 6             ~ "Extremely Contaminated (6<PLI)"
      )
    )
}
# Apply to both background sets and both years
PLI_cats_CENMA <- bind_rows(
  categorize_PLI(PLI_CENMA_2021, "2021"),
  categorize_PLI(PLI_CENMA_2022, "2022")
) |>
  group_by(Year, PLI_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percent = round(100 * n / sum(n), 1)) |>
  ungroup()

PLI_cats_Tume <- bind_rows(
  categorize_PLI(PLI_Tume_2021, "2021"),
  categorize_PLI(PLI_Tume_2022, "2022")
) |>
  group_by(Year, PLI_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percent = round(100 * n / sum(n), 1)) |>
  ungroup()

print(PLI_cats_CENMA)
print(PLI_cats_Tume)


  category_colors <- c(
    "Uncontaminated (PLI<1)"                = "lightgrey",
    "Moderately Contaminated (1<PLI<3)"     = "blue",
    "Heavily Contaminated (3<PLI<6)"        = "orange",
    "Extremely Contaminated (6<PLI)"        = "red2"
  )

# CENMA
ggplot(PLI_cats_CENMA, aes(x = as.factor(Year), y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = category_colors, name = "PLI Category") +
  labs(
    title = "CENMA 2013 % Samples by PLI Category",
    x     = "Project Year",
    y     = "% of Samples"
  ) +
  theme_minimal() +
  theme(
    axis.text    = element_text(size = 11),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

# Tume
ggplot(PLI_cats_Tume, aes(x = as.factor(Year), y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = category_colors, name = "PLI Category") +
  labs(
    title = "Tume et al. 2018 — % Samples by PLI Category",
    x     = "Project Year",
    y     = "% of Samples"
  ) +
  theme_minimal() +
  theme(
    axis.text    = element_text(size = 11),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

### PLI Summary No viz just the table
# PLI CATEGORY SUMMARY — no viz, just the table
# ============================================================

PLI_summary_table <- function(pli_2021, pli_2022, label_col = "sampleLong") {
  bind_rows(
    pli_2021 |> mutate(Year = "2021"),
    pli_2022 |> mutate(Year = "2022")
  ) |>
    mutate(
      PLI_Category = case_when(
        PLI < 1             ~ "Uncontaminated (PLI<1)",
        PLI >= 1 & PLI < 3  ~ "Moderately Contaminated (1<PLI<3)",
        PLI >= 3 & PLI < 6  ~ "Heavily Contaminated (3<PLI<6)",
        PLI >= 6             ~ "Extremely Contaminated (6<PLI)"
      )
    ) |>
    group_by(Year, PLI_Category) |>
    summarize(n = n(), .groups = "drop") |>
    group_by(Year) |>
    mutate(Percent = round(100 * n / sum(n), 1)) |>
    ungroup() |>
    mutate(Result = paste0(n, " (", Percent, "%)")) |>
    select(Year, PLI_Category, Result) |>
    pivot_wider(names_from = Year, values_from = Result)
}

PLI_summary_CENMA <- PLI_summary_table(PLI_CENMA_2021, PLI_CENMA_2022)
PLI_summary_Tume  <- PLI_summary_table(PLI_Tume_2021,  PLI_Tume_2022)

print(PLI_summary_CENMA)
print(PLI_summary_Tume)

#### categorizing CF and Igeo
# CF CATEGORIES
# ============================================================

categorize_CF <- function(df, soil_piv, bg_df, bg_col, year_label) {
  soil_piv |>
    full_join(bg_df, by = "analyte", relationship = "many-to-many") |>
    filter(!is.na(value) & !is.na(.data[[bg_col]])) |>
    mutate(
      Year = year_label,
      CF   = value / .data[[bg_col]],
      CF_Category = case_when(
        CF < 1              ~ "Uncontaminated (CF<1)",
        CF >= 1 & CF < 3    ~ "Moderately Contaminated (1≤CF<3)",
        CF >= 3 & CF < 6    ~ "Heavily Contaminated (3≤CF<6)",
        CF >= 6             ~ "Extremely Contaminated (CF≥6)"
      )
    )
}

CF_summary_table <- function(cf_2021, cf_2022) {
  bind_rows(cf_2021, cf_2022) |>
    group_by(Year, analyte, CF_Category) |>
    summarize(n = n(), .groups = "drop") |>
    group_by(Year, analyte) |>
    mutate(Percent = round(100 * n / sum(n), 1)) |>
    ungroup() |>
    mutate(Result = paste0(n, " (", Percent, "%)")) |>
    select(Year, analyte, CF_Category, Result) |>
    pivot_wider(names_from = Year, values_from = Result, values_fill = "—") |>
    arrange(analyte, CF_Category) |>
    mutate(analyte = ifelse(duplicated(analyte), "", analyte))  # blank repeated analyte names
}

# CENMA
CF_cats_CENMA_2021 <- categorize_CF(NULL, Piv_1_Arica_Soil_2021_redo,
                                    Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2021")
CF_cats_CENMA_2022 <- categorize_CF(NULL, Piv_1_Arica_Soil_2022_redo,
                                    Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2022")
CF_summary_CENMA <- CF_summary_table(CF_cats_CENMA_2021, CF_cats_CENMA_2022)

# Tume
CF_cats_Tume_2021 <- categorize_CF(NULL, Piv_1_Arica_Soil_2021_redo,
                                   Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2021")
CF_cats_Tume_2022 <- categorize_CF(NULL, Piv_1_Arica_Soil_2022_redo,
                                   Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2022")
CF_summary_Tume <- CF_summary_table(CF_cats_Tume_2021, CF_cats_Tume_2022)

print(CF_summary_CENMA)
print(CF_summary_Tume)

# IGEO CATEGORIES
# ============================================================

categorize_Igeo <- function(soil_piv, bg_df, bg_col, year_label) {
  soil_piv |>
    full_join(bg_df, by = "analyte", relationship = "many-to-many") |>
    filter(!is.na(value) & !is.na(.data[[bg_col]])) |>
    mutate(
      Year = year_label,
      Igeo = log2(value / (1.5 * .data[[bg_col]])),
      Igeo_Category = case_when(
        Igeo < 0            ~ "Unpolluted (Igeo<0)",
        Igeo >= 0 & Igeo < 1 ~ "Unpolluted to Moderately Polluted (0≤Igeo<1)",
        Igeo >= 1 & Igeo < 2 ~ "Moderately Polluted (1≤Igeo<2)",
        Igeo >= 2 & Igeo < 3 ~ "Moderately to Strongly Polluted (2≤Igeo<3)",
        Igeo >= 3 & Igeo < 4 ~ "Strongly Polluted (3≤Igeo<4)",
        Igeo >= 4 & Igeo < 5 ~ "Strongly to Extremely Polluted (4≤Igeo<5)",
        Igeo >= 5            ~ "Extremely Polluted (Igeo≥5)"
      )
    )
}

Igeo_summary_table <- function(igeo_2021, igeo_2022) {
  bind_rows(igeo_2021, igeo_2022) |>
    group_by(Year, analyte, Igeo_Category) |>
    summarize(n = n(), .groups = "drop") |>
    group_by(Year, analyte) |>
    mutate(Percent = round(100 * n / sum(n), 1)) |>
    ungroup() |>
    mutate(Result = paste0(n, " (", Percent, "%)")) |>
    select(Year, analyte, Igeo_Category, Result) |>
    pivot_wider(names_from = Year, values_from = Result, values_fill = "—") |>
    arrange(analyte, Igeo_Category) |>
    mutate(analyte = ifelse(duplicated(analyte), "", analyte))  # blank repeated analyte names
}

# CENMA
Igeo_cats_CENMA_2021 <- categorize_Igeo(Piv_1_Arica_Soil_2021_redo,
                                        Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2021")
Igeo_cats_CENMA_2022 <- categorize_Igeo(Piv_1_Arica_Soil_2022_redo,
                                        Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2022")
Igeo_summary_CENMA <- Igeo_summary_table(Igeo_cats_CENMA_2021, Igeo_cats_CENMA_2022)

# Tume
Igeo_cats_Tume_2021 <- categorize_Igeo(Piv_1_Arica_Soil_2021_redo,
                                       Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2021")
Igeo_cats_Tume_2022 <- categorize_Igeo(Piv_1_Arica_Soil_2022_redo,
                                       Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2022")
Igeo_summary_Tume <- Igeo_summary_table(Igeo_cats_Tume_2021, Igeo_cats_Tume_2022)

print(Igeo_summary_CENMA)
print(Igeo_summary_Tume)
