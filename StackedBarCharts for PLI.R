#calling packages
library(ggplot2)
library(patchwork)


#PLI categories column charts makes pls 

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

library(ggplot2)
library(patchwork)

# Shared category order (low → extreme)
pli_cat_order <- c(
  "Uncontaminated (PLI<1)",
  "Moderately Contaminated (1<PLI<3)",
  "Heavily Contaminated (3<PLI<6)",
  "Extremely Contaminated (6<PLI)"
)

pli_colors <- c(
  "Uncontaminated (PLI<1)"                  = "#9FE1CB",
  "Moderately Contaminated (1<PLI<3)"       = "#FAC775",
  "Heavily Contaminated (3<PLI<6)"          = "#D85A30",
  "Extremely Contaminated (6<PLI)"          = "#A32D2D"
)

# Factor both datasets with the same level order
PLI_cats_CENMA <- PLI_cats_CENMA |>
  mutate(PLI_Category = factor(PLI_Category, levels = pli_cat_order))

PLI_cats_Tume <- PLI_cats_Tume |>
  mutate(PLI_Category = factor(PLI_Category, levels = pli_cat_order))

# Shared theme
pli_bar_theme <- theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(size = 10),
    axis.title.x     = element_blank(),
    axis.title.y     = element_text(size = 11),
    plot.title       = element_text(hjust = 0.5, size = 13),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position  = "none",
    strip.text       = element_text(size = 11, face = "bold")
  )

# CENMA plot
p_cenma <- ggplot(PLI_cats_CENMA,
                  aes(x = PLI_Category, y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(Percent, "%")),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ Year, ncol = 2) +
  scale_fill_manual(values = pli_colors) +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(title = "CENMA 2013", y = "% of Samples") +
  pli_bar_theme


p_CENMA_stacked <- ggplot(PLI_cats_CENMA,
                         aes(x = Year, y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(
    values = pli_colors,
    breaks = pli_cat_order,
    name   = "PLI Category"
  ) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title = "CENMA 2013",
    x     = "Project Year",
    y     = "% of Samples"
  ) +
  theme_minimal() +
  theme(
    axis.text       = element_text(size = 11),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 11, face = "bold"),
    legend.text        = element_text(size = 10)
  )

p_CENMA_stacked


# Tume plot
p_tume <- ggplot(PLI_cats_Tume,
                 aes(x = PLI_Category, y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(Percent, "%")),
            vjust = -0.4, size = 3.2, fontface = "bold") +
  facet_wrap(~ Year, ncol = 2) +
  scale_fill_manual(values = pli_colors) +
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
  labs(title = "Tume et al. 2018", y = "% of Samples") +
  pli_bar_theme
p_tume

p_tume_stacked <- ggplot(PLI_cats_Tume,
                         aes(x = Year, y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(
    values = pli_colors,
    breaks = pli_cat_order,
    name   = "PLI Category"
  ) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Tume et al. 2018 PLI",
    x     = "Project Year",
    y     = "% of Samples"
  ) +
  theme_minimal() +
  theme(
    axis.text       = element_text(size = 11),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 11, face = "bold"),
    legend.text        = element_text(size = 10) )

p_tume_stacked

p_tume_stacked_s <- ggplot(PLI_cats_Tume,
                         aes(x = Year, y = Percent, fill = PLI_Category)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = paste0(Percent, "%")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", color = "white") +
  scale_fill_manual(
    values = pli_colors,
    breaks = pli_cat_order,
    name   = "PLI Category"
  ) +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0),
                     labels = scales::label_percent(scale = 1)) +
  labs(
    title = "Tume et al. 2018",
    x     = "Project Year",
    y     = "% of Samples"
  ) +
  theme_minimal() +
  theme(
    axis.text       = element_text(size = 11),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(hjust = 0.5, size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none")
p_tume_stacked_s 


# Combine
(p_CENMA_stacked | p_tume_stacked_s) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "PLI Contamination Class Distribution",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "bottom"
    )
  )|>print()