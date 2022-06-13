library(dplyr)
library(tidyverse)
library(readr)

world_bank <- read_csv("./data/wb_data.csv") %>% janitor::clean_names() %>% 
  mutate(series_code = as.factor(series_code), 
         )

wb <- filter(
  world_bank,
  series_code %in% c(
    "FS.AST.DOMS.GD.ZS",
    "EN.ATM.CO2E.PC",
    "NY.GDP.MKTP.KD.ZG",
    "NY.GNP.PCAP.CD",
    "SP.DYN.TFRT.IN",
    "SP.DYN.LE00.IN",
    "NE.EXP.GNFS.ZS",
    "NE.GDI.TOTL.ZS",
    "NE.IMP.GNFS.ZS",
    "SP.POP.TOTL",
    "EG.USE.ELEC.KH.PC", 
    "EG.USE.PCAP.KG.OE",
    "NY.GDP.MKTP.CD",
    "NY.GDP.DEFL.KD.ZG",
    "MS.MIL.XPND.GD.ZS"
  )
) 
  mutate(series_name = case_when(
                                 series_code == "FS.AST.DOMS.GD.ZS" ~ "domestic_credit", 
                                 series_code == "NY.GDP.MKTP.CD" ~ "gdp_us",
                                 series_code == "NY.GDP.MKTP.KD.ZG" ~ "gdp_growth",
                                 series_code == "NY.GNP.PCAP.CD" ~ "gni", 
                                 series_code == "SP.POP.TOTL" ~ "population",
                                 series_code == "SP.DYN.TFRT.IN" ~ "fertility", 
                                 series_code == "SP.DYN.LE00.IN" ~ "life_exp",
                                 series_code == "NE.EXP.GNFS.ZS" ~ "exports",
                                 series_code == "NE.IMP.GNFS.ZS" ~ "imports",
                                 series_code == "NE.GDI.TOTL.ZS" ~ "capital",
                                 series_code == "NY.GDP.DEFL.KD.ZG" ~ "inflation",
                                 series_code == "EN.ATM.CO2E.PC" ~ "emissions", 
                                 series_code == "EG.USE.ELEC.KH.PC" ~ "electricty_use", 
                                 series_code == "EG.USE.PCAP.KG.OE" ~"oil_use",
                                 series_code == "MS.MIL.XPND.GD.ZS" ~ "military_exp"
                                 ))
wb <- wb %>%
  dplyr::select(country = country_name,
                country_code, 
                series_name, 
                c(5:66)) %>% 
  pivot_longer(cols = 4:65, 
               values_to = "value", 
               names_to = "year", 
               names_prefix = "year_") %>%
  pivot_wider(names_from = series_name) %>% 
  mutate(across(c(domestic_credit, exports, imports, military_exp, capital, inflation), ~ .x/100))


wb_data <- rename(wb, countrycode = country_code) %>%
  mutate(country = gsub("(.*),.*", "\\1", country)) %>%
  mutate(
  country = case_when(
    country == "Hong Kong SAR" ~ "Hong Kong",
    country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country == "Lao PDR" ~ "Laos",
    country == "Macao SAR" ~ "Macao",
    country == "North Macedonia" ~ "Macedonia",
    country == "Slovak Republic" ~ "Slovakia",
    country == "Syrian Arab Republic" ~ "Syria",
    country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
    country == "St. Lucia" ~ "Saint Lucia",
    country == "USA" ~ "United States",
    country == "Viet Nam" ~ "Vietnam",
    TRUE ~ as.character(country)),
  year = as.numeric(year)) %>%
  mutate(country = trimws(country)) %>%
  mutate(country = case_when(countrycode == "COD" ~ "Congo D.R.",
                             countrycode == "KOR" ~ "South Korea",
                             TRUE ~ as.character(country)))
  
write_csv(wb, "wb_data_compiled.csv")


wid_data <- read_csv("./data/wid_data.csv") %>%
  dplyr::select(country, variable, year, value) %>%
  pivot_wider(names_from = "variable") %>% 
  mutate(country = case_when(country == "Cote d'Ivoire" ~ "Cote d'Ivoire",
                             country == "DR Congo" ~ "Congo D.R.",
                             country == "Hong Kong SAR" ~ "Hong Kong",
                             country == "Korea" ~ "South Korea",
                             country == "Lao PDR" ~ "Laos",
                             country == "North Macedonia" ~ "Macedonia",
                             country == "Syrian Arab Republic" ~ "Syria",
                             country == "USA" ~ "United States",
                             country == "Viet Nam" ~ "Vietnam", 
                             TRUE ~ as.character(country))) %>%
  mutate(country = trimws(country))

ifs_imf <-
  read_csv("./data/IFS_timeSeries.csv") %>% janitor::clean_names() %>% 
  arrange(country_name, indicator_name) %>%
  mutate(indicator_code = as.factor(indicator_code))

ifs_imf <- dplyr::filter(ifs_imf, attribute == "Value") %>% 
  filter(indicator_code %in% c("FMB_XDC"))

countrycodes <- group_by(wb_data, country) %>%
  summarize(countrycode = unique(countrycode))

countries <-
  summarize(wid_data, country = unique(country))

merged_countries <- left_join(countries, countrycodes)

wid_wb <-
  left_join(wb_data, merged_countries) %>% left_join(wid_data) %>%
  relocate(year, countrycode, country) %>%
  dplyr::select(-country)

write_csv(wid_wb, "./GitHub/chen_twumasi_eisenbarth/data/wid_wb_compiled.csv")
