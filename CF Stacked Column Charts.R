#calling packages
library(ggplot2)
library(patchwork)


#CF categories column charts makes pls 

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

## trying to make bar chartslibrary(dplyr)
library(ggplot2)
library(patchwork)

# ---- CF categorization ----
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

# ---- Create CF datasets ----
CF_cats_CENMA <- bind_rows(
  categorize_CF(Piv_1_Arica_Soil_2021_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2021"),
  categorize_CF(Piv_1_Arica_Soil_2022_redo, Bg_Levels_CENMA_2013, "bg_level_(mg/kg)_C", "2022")
)

CF_cats_Tume <- bind_rows(
  categorize_CF(Piv_1_Arica_Soil_2021_redo, Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2021"),
  categorize_CF(Piv_1_Arica_Soil_2022_redo, Bg_Levels_Tume_2018, "bg_level_(mg/kg)_T", "2022")
)

# ---- Summarize for plotting ----
CF_cats_CENMA_plot <- CF_cats_CENMA |>
  group_by(Year, CF_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percent = round(100 * n / sum(n), 1)) |>
  ungroup()

CF_cats_Tume_plot <- CF_cats_Tume |>
  group_by(Year, CF_Category) |>
  summarize(n = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percent = round(100 * n / sum(n), 1)) |>
  ungroup()

# ---- Category order + colors ----
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

CF_cats_CENMA_plot <- CF_cats_CENMA_plot |>
  mutate(CF_Category = factor(CF_Category, levels = cf_cat_order))

CF_cats_Tume_plot <- CF_cats_Tume_plot |>
  mutate(CF_Category = factor(CF_Category, levels = cf_cat_order))

# ---- Stacked plots ----
p_CF_CENMA <- ggplot(CF_cats_CENMA_plot,
                     aes(x = Year, y = Percent, fill = CF_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(values = cf_colors, breaks = cf_cat_order, name = "CF Category") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  labs(title = "CENMA 2013", x = "Project Year", y = "% of Samples") +
  theme_minimal() +
  theme(
    axis.text       = element_text(size = 11),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right"
  )

p_CF_Tume <- ggplot(CF_cats_Tume_plot,
                    aes(x = Year, y = Percent, fill = CF_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(values = cf_colors, breaks = cf_cat_order, name = "CF Category") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  labs(title = "Tume et al. 2018", x = "Project Year", y = "% of Samples") +
  theme_minimal() +
  theme(
    axis.text       = element_text(size = 11),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"
  )

# ---- Combine (one legend only) ----
(p_CF_CENMA | p_CF_Tume) +
  plot_annotation(
    title = "CF Contamination Class Distribution",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom"
    )
  )