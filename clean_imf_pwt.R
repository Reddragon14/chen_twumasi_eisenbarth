library(dplyr)
library(tidyr)
library(purrr)
library(boot)
library(data.table)
library(tsibble)
library(fable)
library(fabletools)
library(fable.prophet)
library(feasts)
library(seasonal)
library(forecast)
library(lubridate)
library(prophet)
library(zoo)
library(readr)
library(shiny)
library(ggthemr)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(wesanderson)
library(scales)
library(viridis)
library(ggh4x)
library(fredr)

raspberry <- "#DB2955"
babyblue <- "#47AFFF"
prussian <- "#113255"
sapphire <- "#255F85"
mint <- "#B2EDC5"
celadon <- "#5F8DBF"
viridian <- "#5A876F"
khaki <- "#9fae84"
turq <- "#76E7CD"
emerald <- "#32936F"
colombia <- "#C7E8F3"
violet <- "#AA78A6"
jeans <- "#418B82"
sparkle <- "#2F5075"
sky <- "#ABDDED"
pale_mint <- "#73BFB0"
jungle <- "#193832"
amazon <- "#317658"
bedazzled <- "#1E569B"
colors <- c(jeans, celadon, sapphire, mint, raspberry, violet)
cools <- c(pale_mint, emerald, colombia, sapphire, khaki, sparkle, amazon, sky, turq, bedazzled, viridian, prussian, jungle)

#' load in global debt data from IMF
ggd <- haven::read_dta("./debt/data/imf_gdd.dta") %>% haven::zap_labels() %>%
  select(ifscode, year, country, pvd = pvd_all, hhd = hh_all, gg, cg, gdp = ngdp) %>%
  mutate(across(c(4:7), ~ .x/100))
#' fix strings on country column, we will use this column to merge 
#' and match codes between Penn World Tables and the IMF dataset
ggd <- mutate(ggd, country = gsub("&", "and", country)) %>%
  mutate(
    country = case_when(
      country == "C.A.R." ~ "Central African Republic",
      country == "Congo, Dem. Rep. of" ~ "Congo D.R.",
      country == "Kyrgyz Republic" ~ "Kyrgyzstan",
      country == "São Tomé and Príncipe" ~ "Sao Tome and Principe",
      country == "Slovak Republic" ~ "Slovakia",
      country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
      country == "St. Lucia" ~ "Saint Lucia",
      country == "Taiwan Province of China" ~ "Taiwan",
      country == "U.A.E." ~ "United Arab Emirates",
      TRUE ~ country,
    )
  ) %>%
  mutate(country = gsub("^(.*?),.*", "\\1", country)) %>%
  mutate(country = trimws(country))
#' fix strings on country column, we will use this column to merge 
#' and match codes between Penn World Tables and the IMF dataset
pwt <- haven::read_dta("./debt/data/pwt100.dta") %>%
  mutate(country = sub('.*,\\s*', '', country)) %>%
  mutate(country = sub("[\\(\\)].*", "", country)) %>%
  mutate(country = case_when(country == "Côte d'Ivoire" ~ "Cote D'Ivoire",
                             country == "D.R. of the Congo" ~ "Congo D.R.",
                             country == "Lao People's DR" ~ "Laos",
                             country == "North Macedonia" ~ "Macedonia",
                             country == "Republic of Korea" ~ "Korea",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "Viet Nam" ~ "Vietnam",
                             country == "U.R. of Tanzania: Mainland" ~ "Tanzania",
                             TRUE ~ country,
  )) %>%
  mutate(country = trimws(country))
#' retrieve unique set of countrycodes in Penn World Tables
countrycodes <- group_by(pwt, country) %>%
  summarize(countrycode = unique(countrycode))
#' retrieve unique set of ifscode in IMF Global Debt Database tables
ifscodes <- group_by(ggd, country) %>%
  summarize(ifscode = unique(ifscode))
#' merge codes 
ccodes <- inner_join(ifscodes, countrycodes)
#' merge codes on ggd
df <- inner_join(ggd, ccodes, by = c("country", "ifscode")) %>% 
  relocate(year, ifscode, countrycode, country)

pwt_vars <- c("cgdpe",
              "cgdpo",
              "rgdpe",
              "rgdpo",
              "pop",
              "emp",
              "hc",
              "rconna",
              "ctfp",
              "labsh",
              "pl_c",
              "pl_i",
              "pl_g",
              "pl_x",
              "pl_m")

pwt_subset <- select(pwt, year, countrycode, country, all_of(pwt_vars))


df <- left_join(df, pwt_subset, by = c("year", "country", "countrycode")) %>% 
  arrange(countrycode, year) %>% 
  filter(year < 2020)

write_csv(df, "./data/merged_debt.csv")
