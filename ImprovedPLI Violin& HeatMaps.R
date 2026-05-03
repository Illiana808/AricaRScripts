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
add_pli_benchmarks <- function(p) {
  p +
    geom_hline(aes(yintercept = 1, color = "Moderately Contaminated (1<PLI<3)"),
               linewidth = 0.4, linetype = "dashed") +
    geom_hline(aes(yintercept = 3, color = "Heavily Contaminated (3<PLI<6)"),
               linewidth = 0.4, linetype = "dashed") +
    geom_hline(aes(yintercept = 6, color = "Extremely Contaminated (6<PLI)"),
               linewidth = 0.4, linetype = "dashed") +
    scale_color_manual(values = pli_benchmark_colors) +
    guides(
      color = guide_legend(title = "Benchmark PLIs",
                           override.aes = list(linetype = "dashed", linewidth = 1)),
      fill  = guide_legend(title = "Project Year")
    )
}

#CENMA
p1 <- ggplot(Consolidated_CENMA_PLI, aes(x = as.factor(Year), y = PLI)) +
  geom_violin(fill = "orange", alpha = 0.6) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.7) +
  labs(x = "Project Year", y = "CENMA 2013 PLI") +
  scale_y_continuous(breaks = seq(0, 12, by = 1), limits = c(0, 12)) +
  theme_minimal() +
  pli_theme

p1 <- add_pli_benchmarks(p1)
plot(p1)

#Tume
p2 <- ggplot(Consolidated_Tume_PLI, aes(x = as.factor(Year), y = PLI)) +
  geom_violin(fill = "orchid", alpha = 0.6) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.7) +
  labs(x = "Project Year", y = "Tume et al. 2018 PLI") +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  theme_minimal() +
  pli_theme

p2 <- add_pli_benchmarks(p2)
p2

# ============================================================
# HEATMAPS — CF (geomean!)
sample_key_2021 <- Piv_1_Arica_Soil_2021_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Create a Sample_Number key from 2022 data
sample_key_2022 <- Piv_1_Arica_Soil_2022_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

CF_heatmap_CENMA_summary <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> mutate(Year = "2022")
) |>
  full_join(Bg_Levels_CENMA_2013, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_C`)) |>
  mutate(CF = value / `bg_level_(mg/kg)_C`) |>
  group_by(Year, analyte) |>
  summarize(CF_geomean = geoMean(CF), .groups = "drop")

#Use this one for better gradient at low values
CF_heatmap_CENMA_plot <- ggplot(CF_heatmap_CENMA_summary,
                                aes(x = Year, y = analyte, fill = CF_geomean)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 10 ,15, 20)),  # stretched near 0-1 for visibility
    limits  = c(0, 20),
    oob     = scales::squish,
    name    = "CF(Geometric Mean)"
  ) +
  geom_text(aes(label = round(CF_geomean, 1) ),  # dark text on light tiles, white on dark
            size = 3.5, fontface = "bold") +
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

#isolated for combination w patchwork 

CF_heatmap_CENMA_tg <- ggplot(CF_heatmap_CENMA_summary,
                              aes(x = Year, y = analyte, fill = CF_geomean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(CF_geomean, 1)),
            size = 3.5, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 10 ,15, 20)),
    limits  = c(0, 20),
    oob     = scales::squish,
    name    = "CF(Geometric Mean)"
  ) +
  labs(title = "CENMA 2013")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),  # Changed from element_blank()
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # Remove grey background
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    plot.background = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )

plot(CF_heatmap_CENMA_tg)



##Tume
sample_key_2021 <- Piv_1_Arica_Soil_2021_redo |>
  distinct(sampleLong) |>
  arrange(sampleLong) |>
  mutate(Sample_Number = row_number())

# Create a Sample_Number key from 2022 data
CF_heatmap_Tume_summary <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> mutate(Year = "2022")
) |>
  full_join(Bg_Levels_Tume_2018, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_T`)) |>
  mutate(CF = value / `bg_level_(mg/kg)_T`) |>
  group_by(Year, analyte) |>
  summarize(CF_geomean = geoMean(CF), .groups = "drop")

CF_heatmap_Tume_plot <- ggplot(CF_heatmap_Tume_summary,
                               aes(x = Year, y = analyte, fill = CF_geomean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(CF_geomean, 1)),  # aes() closed here
            size = 3.5, fontface = "bold") +     # size/fontface outside aes()
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 10 ,15, 20)),
    limits  = c(0, 20),
    oob     = scales::squish,
    name    = "CF(Geometric Mean)"
  ) +
  theme_minimal() +
  labs(
    title = "Tume 2018 Contamination Factor Heat Map",
    x     = "Sample",
    y     = "Analyte"
  ) +
  theme(
    axis.text.x  = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y  = element_text(size = 10),
    axis.title   = element_text(size = 13),
    plot.title   = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid   = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 9)
  )

#simple for patchwork
CF_heatmap_Tume_tg <- ggplot(CF_heatmap_Tume_summary,
                              aes(x = Year, y = analyte, fill = CF_geomean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(CF_geomean, 1)),
            size = 3.5, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("white", "lightcyan", "cyan2", "deepskyblue", "dodgerblue4"),
    values  = scales::rescale(c(0, 1, 5, 10 ,15, 20)),
    limits  = c(0, 20),
    oob     = scales::squish,
    name    = "CF(Geometric Mean)"
  ) +
  labs(title = "Tume et al. 2018")+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),  # Changed from element_blank()
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid = element_blank(),
    panel.background = element_blank(),  # Remove grey background
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    plot.background = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )

plot(CF_heatmap_Tume_tg)
#combing plots

library(patchwork)

(CF_heatmap_CENMA_tg | CF_heatmap_Tume_tg) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Contamination Factor Comparison",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  ) &
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom"
  )

# Igeo heat maps

# ============================================================
# HEATMAPS — Igeo (mean! instead of geomean)
# ============================================================

# CENMA Igeo Heatmap Summary (using mean)
Igeo_heatmap_CENMA_summary <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> mutate(Year = "2022")
) |>
  full_join(Bg_Levels_CENMA_2013, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_C`)) |>
  mutate(Igeo = log2(value / (1.5 * `bg_level_(mg/kg)_C`))) |>
  group_by(Year, analyte) |>
  summarize(Igeo_mean = mean(Igeo, na.rm = TRUE), .groups = "drop")

# CENMA Igeo heatmap (simple for patchwork)
Igeo_heatmap_CENMA_tg <- ggplot(Igeo_heatmap_CENMA_summary,
                                aes(x = Year, y = analyte, fill = Igeo_mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Igeo_mean, 2)),
            size = 3.5, fontface = "plain") +
  scale_fill_gradientn(
    colors = c("steelblue", "deepskyblue", "white", "orange", "darkred"),
    values = scales::rescale(c(-5, -3, -2, -1, 0, 1, 2, 3, 4, 5)),
    limits = c(-5, 5),
    oob = scales::squish,
    name = "Igeo (Mean)"
  ) +
  labs(title = "CENMA 2013") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.background = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )

plot(Igeo_heatmap_CENMA_tg)

# Tume Igeo Heatmap Summary (using mean)
Igeo_heatmap_Tume_summary <- bind_rows(
  Piv_1_Arica_Soil_2021_redo |> mutate(Year = "2021"),
  Piv_1_Arica_Soil_2022_redo |> mutate(Year = "2022")
) |>
  full_join(Bg_Levels_Tume_2018, by = "analyte", relationship = "many-to-many") |>
  filter(!is.na(value) & !is.na(`bg_level_(mg/kg)_T`)) |>
  mutate(Igeo = log2(value / (1.5 * `bg_level_(mg/kg)_T`))) |>
  group_by(Year, analyte) |>
  summarize(Igeo_mean = mean(Igeo, na.rm = TRUE), .groups = "drop")

# Tume Igeo heatmap (simple for patchwork)
Igeo_heatmap_Tume_tg <- ggplot(Igeo_heatmap_Tume_summary,
                               aes(x = Year, y = analyte, fill = Igeo_mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Igeo_mean, 2)),
            size = 3.5, fontface = "plain") +
  scale_fill_gradientn(
    colors = c("steelblue", "deepskyblue", "white", "orange", "darkred"),
    values = scales::rescale(c(-5, -3, -2, -1, 0, 1, 2, 3, 4, 5)),
    limits = c(-5, 5),
    oob = scales::squish,
    name = "Igeo (Mean)"
  ) +
  labs(title = "Tume et al. 2018") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "plain"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 9, color = "grey40"),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.background = element_blank(),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )

plot(Igeo_heatmap_Tume_tg)

# ============================================================
# COMBINE IGEO HEATMAPS WITH PATCHWORK
# ============================================================

library(patchwork)

(Igeo_heatmap_CENMA_tg | Igeo_heatmap_Tume_tg) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Igeo Comparison (Mean Values)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
    )
  ) &
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "bottom"
  )
