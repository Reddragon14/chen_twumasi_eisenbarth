library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)

data_path <- "./data/"

#' load compiled World Inequality and World Bank data
wid_wb <- read_csv(file.path(data_path, "wid_wb_compiled.csv"))

#' Combine and merge with compiled IMF and PWT data
df <- read_csv("./data/merged_debt.csv") %>%
  #' two hacks for Cote d'Ivoire and Hong Kong
  #' to ensure that the data merges correctly
  mutate(country = case_when(countrycode == "CIV" ~"Cote d'Ivoire",
                             countrycode == "HKG" ~ "Hong Kong",
                             TRUE ~ country)) %>%  
  left_join(wid_wb)
#' create lmh - an indicator variable based on current World Bank
#' classifications of income levels
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

#' merge back with original dataframe
df <- left_join(df, incomes) %>%
  relocate(lmh, .after = "country")

#' load countries with geographic regions 
countries <- read_csv("./data/final_countries.csv")
#' merge with dataframe
df <- left_join(df, countries) %>%
  relocate(region, .after = "country")
#' create an indicator variable for European Union status
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
#' take the above list of EU countries and create the binary (dummy) variable
df <- mutate(df, country = factor(country)) %>%
  mutate(european_union = if_else(country %in% eu_countries, 1, 0)) %>%
  relocate(european_union, .after = region)

#' save final dataset
write_csv(df, "./GitHub/chen_twumasi_eisenbarth/data/merged_data.csv")
