library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

data_path <- "./data/"



df <- read_csv("./data/merged_debt.csv") %>%
  mutate(country = case_when(countrycode == "CIV" ~"Cote d'Ivoire",
                             countrycode == "HKG" ~ "Hong Kong",
                             TRUE ~ country)) %>%  
  left_join(wid_wb)

incomes <- 
  group_by(df, country) %>%
  summarize(gni = mean(gni, na.rm = TRUE)) %>%
  mutate(incomes, 
         lmh = case_when(gni <= 1045 ~ "Low income",
                         gni >= 1046 & gni <= 4095 ~ "Lower-Middle Income",
                         gni >= 4096 & gni <= 12695 ~ "Upper-Middle Income",
                         gni >= 12696 ~ "High Income")) %>% 
  mutate(lmh = if_else(country == "Taiwan", "High Income", lmh)) %>%
  dplyr::select(country, lmh)

df <- left_join(df, incomes) %>%
  relocate(lmh, .after = "country")

countries <- read_csv("./data/final_countries.csv")

df <- left_join(df, countries) %>%
  relocate(region, .after = "country")

eu_countries <- c("Austria", 
                  "Belgium", 
                  "Bulgaria", 
                  "Croatia", 
                  "Cyprus",
                  "Czech Republic",
                  "Denmark",
                  "Estonia",
                  "Finland", 
                  "France",
                  "Germany",
                  "Greece", 
                  "Hungary", 
                  "Ireland", 
                  "Italy", 
                  "Latvia", 
                  "Lithuania", 
                  "Luxembourg", 
                  "Malta", 
                  "Netherlands", 
                  "Poland", 
                  "Portugal", 
                  "Romania", 
                  "Slovakia", 
                  "Slovenia", 
                  "Spain",
                  "Sweden"
)

df <- mutate(df, country = factor(country)) %>%
  mutate(european_union = if_else(country %in% eu_countries, 1, 0)) %>%
  relocate(european_union, .after = region)


write_csv(df, "./data/merged_data.csv")