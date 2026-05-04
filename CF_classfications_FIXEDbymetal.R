library(dplyr)
library(ggplot2)
library(patchwork)
library(stringr)

# ---- Categorize CF (already correct) ----
categorize_CF <- function(soil_piv, bg_df, bg_col, year_label) {
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

# ---- Build datasets ----
CF_CENMA <- bind_rows(
  categorize_CF(Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2021"),
  categorize_CF(Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2022")
)

CF_Tume <- bind_rows(
  categorize_CF(Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2021"),
  categorize_CF(Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2022")
)

# ---- Summarize per element ----
CF_CENMA_plot <- CF_CENMA |>
  group_by(Year, analyte, CF_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year, analyte) |>
  mutate(Percent = 100 * n / sum(n)) |>
  ungroup()

CF_Tume_plot <- CF_Tume |>
  group_by(Year, analyte, CF_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year, analyte) |>
  mutate(Percent = 100 * n / sum(n)) |>
  ungroup()

# ---- Order + colors ----
cf_cat_order <- c(
  "Uncontaminated (CF<1)",
  "Moderately Contaminated (1≤CF<3)",
  "Heavily Contaminated (3≤CF<6)",
  "Extremely Contaminated (CF≥6)"
)

cf_colors <- c(
  "Uncontaminated (CF<1)"            = "#9FE1CB",
  "Moderately Contaminated (1≤CF<3)" = "#FAC775",
  "Heavily Contaminated (3≤CF<6)"    = "#D85A30",
  "Extremely Contaminated (CF≥6)"    = "#A32D2D"
)

CF_CENMA_plot <- CF_CENMA_plot |>
  mutate(CF_Category = factor(CF_Category, levels = cf_cat_order))

CF_Tume_plot <- CF_Tume_plot |>
  mutate(CF_Category = factor(CF_Category, levels = cf_cat_order))

# ---- Plot function ----
plot_CF <- function(df, title_text, show_legend = TRUE) {
  ggplot(df, aes(x = analyte, y = Percent, fill = CF_Category)) +
    geom_col(width = 0.7) +
    geom_text(
      aes(label = paste0(round(Percent, 1), "%")),
      position = position_stack(vjust = 0.5),
      size = 3,
      fontface = "bold",
      color = "white"
    ) +
    facet_wrap(~ Year, ncol = 2) +
    scale_fill_manual(values = cf_colors, breaks = cf_cat_order, name = "CF Category") +
    scale_y_continuous(labels = scales::label_percent(scale = 1), expand = c(0, 0)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    labs(
      title = title_text,
      x = "Analyte",
      y = "% of Samples"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = ifelse(show_legend, "bottom", "none"),
      panel.grid.major.x = element_blank()
    )
}

# ---- Create plots ----
p_CF_CENMA <- plot_CF(CF_CENMA_plot, "CENMA 2013", TRUE)
p_CF_Tume  <- plot_CF(CF_Tume_plot,  "Tume et al. 2018", FALSE)

# ---- Combine ----
(p_CF_CENMA | p_CF_Tume) +
  plot_annotation(
    title = "CF Contamination Distribution by Element",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
    )
  )

# ---- Create plots (with explicit class titles) ----
p_CF_CENMA <- plot_CF(CF_CENMA_plot, "CENMA 2013 Contamination Factor Classes", TRUE) 

p_CF_Tume <- plot_CF(CF_Tume_plot, "Tume et al. 2018 Contamination Factor Classes", TRUE) 

# ---- Print separately ----
p_CF_CENMA
p_CF_Tume