library(rstatix)
library(tidyverse)
library(readxl)
library(fitdistrplus)

#Import & pivot
Arica_Soil_Table_Redone_4_stats <- read_excel(
  "~/Downloads/Arica Soil Table Redone.xlsx", 
  sheet = "Both Years Together 4 stats"
)

long_data <- 
  tibble(Arica_Soil_Table_Redone_4_stats) |>
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  mutate(log_value = log(value))

long_data_filtered <- long_data |>
  filter(!Analyte %in% c("barium", "beryllium"))|>  # keep year-groups with at least 3 real values
  ungroup()

##helper function for stats
safe_shapiro <- function(x) {
x <- x[!is.na(x) & is.finite(x)] #IDs NA or infinite values
if (length(x) < 3) return(NA_real_) ## Need n>3 for shap test, if n is too low, IDs as NA
shapiro.test(x)$p.value
}

##Identifying appropriate stat test
normality_summary <- long_data_filtered |>
  group_by(Analyte, sampling_year) |>
  summarise(
    p_raw = safe_shapiro(value),
    p_log = safe_shapiro(log_value),
    n     = n(),
    .groups = "drop"
  ) |>
  group_by(Analyte) |>
  filter(n_distinct(sampling_year) == 2) |>  # ← drop analytes with only 1 valid year
  summarise(
    pass_raw = all(p_raw > 0.05, na.rm = TRUE),
    pass_log = all(p_log > 0.05, na.rm = TRUE),
    min_n    = min(n),
    .groups  = "drop"
  ) |>
  mutate(
    test_to_use = case_when(
      min_n < 3 ~ "Kruskal-Wallis",
      pass_raw  ~ "t-test (raw)",
      pass_log  ~ "t-test (log)",
      TRUE      ~ "Kruskal-Wallis"
    )
  )

print(normality_summary)

# ── Run appropriate test for each analyte based on normality results ──────────
stat_results <- list()

for (analyte in unique(long_data_filtered$Analyte)) {
  
  analyte_data <- long_data_filtered |> filter(Analyte == analyte)
  test_to_use  <- normality_summary |> 
    filter(Analyte == analyte) |> 
    pull(test_to_use)
  
  result <- switch(test_to_use,
                   "t-test (raw)" = t.test(value     ~ sampling_year, data = analyte_data),
                   "t-test (log)" = t.test(log_value ~ sampling_year, data = analyte_data),
                   "Kruskal-Wallis" = kruskal.test(value ~ sampling_year, data = analyte_data)
  )
  
  stat_results[[analyte]] <- list(test = test_to_use, result = result)
}

# ── Final summary table ───────────────────────────────────────────────────────
final_summary <- data.frame(
  analyte   = names(stat_results),
  test_used = sapply(stat_results, \(x) x$test),
  p_value   = sapply(stat_results, \(x) signif(x$result$p.value, 3 )),
  sig       = sapply(stat_results, \(x) ifelse(x$result$p.value < 0.05, "*", "ns"))
)

as.data.frame(final_summary)


# Identifying the type of distribution for each Analyte
analytes <- unique(long_data$Analyte)
dist_results <- list()

for (analyte in analytes) {
  
  vals <- long_data |> 
    filter(Analyte == analyte) |> 
    pull(value)
  
  vals <- vals[is.finite(vals) & !is.na(vals) & vals > 0]
  
  cat("\n", strrep("=", 40), "\n")
  cat("Analyte:", toupper(analyte), "\n")
  
  # ── Try each distribution, skip if it fails ──────────────────────────────── 
  distributions <- c("norm", "lnorm", "exp", "pois", "cauchy", 
                     "gamma", "logis", "nbinom", "geom", "beta", "weibull")
  
  aic_rows <- list()
  
  for (dist in distributions) {
    tryCatch({
      fit <- fitdist(vals, dist)
      aic_rows[[dist]] <- data.frame(distribution = dist, AIC = fit$aic)
    }, error = function(e) {
      cat(sprintf("  skipped %-10s : %s\n", dist, e$message))
    })
  }
  
  # ── Combine & rank by AIC ─────────────────────────────────────────────────
  aic_table <- bind_rows(aic_rows) |> arrange(AIC)
  print(aic_table)
  
  dist_results[[analyte]] <- aic_table
}

# ── Best fit summary across all analytes ──────────────────────────────────────
best_fit_summary <- data.frame(
  analyte  = names(dist_results),
  best_fit = sapply(dist_results, \(x) x$distribution[1]),
  AIC      = sapply(dist_results, \(x) round(x$AIC[1], 2))
) |> arrange(best_fit)

as.data.frame(best_fit_summary)







