#Stats tests for Arica Soils
library(ggplot2)
library(multcompView)
library(dplyr)
library(ggpubr)
library(tidyverse)

# sorting data 4 statistical tests (adding log column)
Piv_Arica_Soil_4_stats <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  group_by(Analyte, sampling_year)|>
  unite("Metal_by_Sample_Year", c("Analyte", "sampling_year"), sep = "(", remove = TRUE) %>%
  mutate(Metal_by_Sample_Year = paste0(Metal_by_Sample_Year, ")")) |>
  mutate (log = log(value) )  |> print()

#Making dfs for each analyte & yr

# Check normal distribution for arsenic

#Make As 2021 df
check_As_2021<- "arsenic(2021)"

Piv_Arica_Soil_4_stats_As_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_As_2021) |> 
  print()

#Make As 2022 df
check_As_2022<- "arsenic(2022)"

Piv_Arica_Soil_4_stats_As_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_As_2022) |> 
  print()

#shapiro test for arsenic
shapiro_test_As_2021 <- shapiro.test(Piv_Arica_Soil_4_stats_As_2021$value) |> print()
log_As_2021 <- log(Piv_Arica_Soil_4_stats_As_2021$value)
shapiro_test_As_2021_2 <- shapiro.test(log_As_2021) |> print()

shapiro_test_As_2022 <- shapiro.test(Piv_Arica_Soil_4_stats_As_2022$value) |> print()
log_As_2022 <- log(Piv_Arica_Soil_4_stats_As_2022$value)
shapiro_test_As_2022_2 <- shapiro.test(log_As_2022) |> print()

#result, its not non-parametric, run Kruskal Wallis
#Use arsenic_data string from boxplots since this alr smushes values together 

# Step 2: Create the grouping factor (for years)
As_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "arsenic") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_As <- kruskal.test(value ~ sampling_year, data = As_Kruskal_set) |> print()

----
  # Check normal distribution for aluminum
  
#Make As 2021 df

check_Al_2021<- "aluminum(2021)"

Piv_Arica_Soil_4_stats_Al_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Al_2021) |> 
  print()

#Make As 2022 df
check_Al_2022<- "aluminum(2022)"

Piv_Arica_Soil_4_stats_Al_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Al_2022) |> 
  print()

#shapiro test for arsenic
shapiro_test_Al_2021 <- shapiro.test(Piv_Arica_Soil_4_stats_Al_2021$value) |> print()
log_Al_2021 <- log(Piv_Arica_Soil_4_stats_Al_2021$value)
shapiro_test_Al_2021_2 <- shapiro.test(log_Al_2021) |> print()

#Niceeee huge p value when log transformed (p-value = 0.9741) after log, let's check 2022

shapiro_test_Al_2022 <- shapiro.test(Piv_Arica_Soil_4_stats_Al_2022$value) |> print()
log_Al_2022 <- log(Piv_Arica_Soil_4_stats_Al_2022$value)
shapiro_test_Al_2022_2 <- shapiro.test(log_Al_2022) |> print()

#result, its pretty normal for both yrs p>0.05, run t test w/ log transformed data


# Step 2: Create the grouping factor (for years); redo w/ log values
Al_t_set <- 
  tibble(  tibble(Arica_Soil_data_basic_stats_v_2) |> 
            pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>         
            mutate (log = log(value) ) |>
            filter(Analyte == "aluminum") |>
            group_by(Analyte, sampling_year) )|> print()

t_test_Al <- t.test(log ~ sampling_year, data = Al_t_set) |> print()

-------
# Check normal distribution for cadmium
  
#Make Cd 2021 df

check_Cd_2021<- "cadmium(2021)"

Piv_Arica_Soil_4_stats_Cd_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cd_2021) |> 
  print()

#Make Cd 2022 df
check_Cd_2022<- "cadmium(2022)"

Piv_Arica_Soil_4_stats_Cd_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cd_2022) |> 
  print()

#shapiro test for cadmium
shapiro_test_Cd_2021 <- shapiro.test(Piv_Arica_Soil_4_stats_Cd_2021$value) |> print()
log_Cd_2021 <- log(Piv_Arica_Soil_4_stats_Cd_2021$value)
shapiro_test_Cd_2021_2 <- shapiro.test(log_Cd_2021) |> print()

#Nice, after log p-value = 0.263

shapiro_test_Cd_2022 <- shapiro.test(Piv_Arica_Soil_4_stats_Cd_2022$value) |> print()
log_Cd_2022 <- log(Piv_Arica_Soil_4_stats_Cd_2022$value)
shapiro_test_Cd_2022_2 <- shapiro.test(log_Cd_2022) |> print()

#Failed Shap 2022 p-value = 2.19e-07 --> proceed to Krustkal Wallis

# Step 2: Create the grouping factor (for years)
Cd_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "cadmium") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_Cd <- kruskal.test(value ~ sampling_year, data = Cd_Kruskal_set) |> print()

-----
# Check normal distribution for chromium 
# Make Cr 2021 df

check_Cr_2021<- "chromium(2021)"

Cr_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cr_2021) |> 
  print()

#Make Cr 2022 df
check_Cr_2022<- "chromium(2022)"

Cr_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cd_2022) |> 
  print()

#shapiro test for chromium
shapiro_test_Cr_2021 <- shapiro.test(Cr_2021$value) |> print()
log_Cr_2021 <- log(Cr_2021$value)
shapiro_test_Cr_2021_2 <- shapiro.test(log_Cr_2021) |> print()

#Nice, after log p-value = 0.2652 after log

shapiro_test_Cr_2022 <- shapiro.test(Cr_2022$value) |> print()
log_Cr_2022 <- log(Cr_2022$value)
shapiro_test_Cr_2022_2 <- shapiro.test(log_Cr_2022) |> print()

#Failed Shap 2022 p-value = 2.19e-07 --> proceed to Krustkal Wallis

# Step 2: Create the grouping factor (for years)
Cr_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "chromium") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_Cr <- kruskal.test(value ~ sampling_year, data = Cr_Kruskal_set) |> print()

-------------
  
# Check normal distribution for copper
#Make Cu 2021 df
check_Cu_2021<- "copper(2021)"

Cu_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cu_2021) |> 
  print()

#Make Cu 2022 df
check_Cu_2022<- "copper(2022)"

Cu_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Cu_2022) |> 
  print()

#shapiro test for copper
shapiro_test_Cu_2021 <- shapiro.test(Cu_2021$value) |> print()
log_Cu_2021 <- log(Cu_2021$value)
shapiro_test_Cu_2021_2 <- shapiro.test(log_Cu_2021) |> print()

#Failed;  p= 0.04263, even after log

shapiro_test_Cu_2022 <- shapiro.test(Cu_2022$value) |> print()
log_Cu_2022 <- log(Cu_2022$value)
shapiro_test_Cu_2022_2 <- shapiro.test(log_Cu_2022) |> print()

#Failed Shap 2021 --> proceed to Krustkal Wallis

# Step 2: Create the grouping factor (for years)
Cu_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "copper") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_Cu <- kruskal.test(value ~ sampling_year, data = Cu_Kruskal_set) |> print()

----
# Check normal distribution for lead
# Make Pb 2021 df

check_Pb_2021<- "lead(2021)"

Pb_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Pb_2021) |> 
  print()

#Make Pb 2022 df
check_Pb_2022<- "lead(2022)"

Pb_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Pb_2022) |> 
  print()

#shapiro test for lead
shapiro_test_Pb_2021 <- shapiro.test(Pb_2021$value) |> print()
log_Pb_2021 <- log(Pb_2021$value)
shapiro_test_Pb_2021_2 <- shapiro.test(log_Pb_2021) |> print()

#Nice, very high p value for 2021 lead

shapiro_test_Pb_2022 <- shapiro.test(Pb_2022$value) |> print()
log_Pb_2022 <- log(Pb_2022$value)
shapiro_test_Pb_2022_2 <- shapiro.test(log_Pb_2022) |> print()

#Failed Shap 2022 --> proceed to Krustkal Wallis

# Step 2: Create the grouping factor (for years)
Pb_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "lead") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_Pb <- kruskal.test(value ~ sampling_year, data = Pb_Kruskal_set) |> print()
-----
# Check normal distribution for manganese
# Make Mn 2021 df

check_Mn_2021<- "manganese(2021)"

Mn_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Mn_2021) |> 
  print()

#Make Mn 2022 df
check_Mn_2022<- "manganese(2022)"

Mn_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Mn_2022) |> 
  print()

#shapiro test for manages
shapiro_test_Mn_2021 <- shapiro.test(Mn_2021$value) |> print()
log_Mn_2021 <- log(Mn_2021$value)
shapiro_test_Mn_2021_2 <- shapiro.test(log_Mn_2021) |> print()

#Nice,  high p value for 2021 manganese both log & reg (slightly higher for log transformed data tho)

shapiro_test_Mn_2022 <- shapiro.test(Mn_2022$value) |> print()
log_Mn_2022 <- log(Mn_2022$value)
shapiro_test_Mn_2022_2 <- shapiro.test(log_Mn_2022) |> print()

#Failed shapiro both times w/ 2022 values

# Step 2: Create the grouping factor (for years)
Mn_Kruskal_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "manganese") |>
  group_by(Analyte, sampling_year)|> print()

kw_test_Mn <- kruskal.test(value ~ sampling_year, data = Mn_Kruskal_set) |> print()

######
# Check normal distribution for nickel
# Make Ni 2021 df

check_Ni_2021<- "nickel(2021)"

Ni_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Ni_2021) |> 
  print()

#Make Ni 2022 df
check_Ni_2022<- "nickel(2022)"

Ni_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Ni_2022) |> 
  print()

#shapiro test for nickel 2021
shapiro_test_Ni_2021 <- shapiro.test(Ni_2021$value) |> print()
log_Ni_2021 <- log(Ni_2021$value)
shapiro_test_Ni_2021_2_olscoo <- shapiro.test(log_Ni_2021)|> print()
shapiro_test_Ni_2021_2 <- shapiro.test(Ni_2021$log) |> print() #better

#Nice,  high p value for 2021 nickel log transformed

shapiro_test_Ni_2022 <- shapiro.test(Ni_2022$value) |> print()
shapiro_test_Ni_2022_2 <- shapiro.test(Ni_2022$log) |> print()

#Barely passed w/ Shapiro w/ log transformed data; kinda small p value buts its greater than 0.05

# Step 2: Create the grouping factor (for years)
Ni_t_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "nickel") |>
  group_by(Analyte, sampling_year)|>
  mutate( log = log(value) ) |> print()

t_test_Ni <- t.test(log ~ sampling_year, data = Ni_t_set) |> print()

######
# Check normal distribution for zinc
# Make Zn 2021 df

check_Zn_2021<- "zinc(2021)"

Zn_2021 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Zn_2021) |> 
  print()

#Make Zn 2022 df
check_Zn_2022<- "zinc(2022)"

Zn_2022 <- Piv_Arica_Soil_4_stats|> 
  filter(Metal_by_Sample_Year == check_Zn_2022) |> 
  print()

#shapiro test for zinc 2021
shapiro_test_Zn_2021 <- shapiro.test(Zn_2021$value) |> print()
shapiro_test_Zn_2021_2 <- shapiro.test(Zn_2021$log) |> print() 

#Nice,  high p value for 2021 zinc log transformed

shapiro_test_Zn_2022 <- shapiro.test(Zn_2022$value) |> print()
shapiro_test_Zn_2022_2 <- shapiro.test(Zn_2022$log) |> print()

#Barely passed w/ Shapiro w/ log transformed data; kinda small p value buts its greater than 0.05; 
#honestly imma do both & see which is lower

# Step 2: Create the grouping factor (for years)
Ni_stat_set <- 
  tibble(Arica_Soil_data_basic_stats_v_2) |> 
  pivot_longer(cols = aluminum:vanadium, names_to = "Analyte", values_to = "value") |>
  filter(Analyte == "nickel") |>
  group_by(Analyte, sampling_year)|>
  mutate( log = log(value) ) |> print()

t_test_Ni <- t.test(log ~ sampling_year, data = Ni_stat_set) |> print()
kw_test_Ni <-kruskal.test(log ~ sampling_year, data = Ni_stat_set) |> print()

#take KW test value because its a bit more conservative

