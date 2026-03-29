library(dplyr)
library(tidyverse)
library(ggplot2)
library(extrafont)
library(ggh4x)
library(ggthemes)
library(scales)
library(ggpubr)
library(car)
library(ARDL)
library(estimatr)
library(feasts)
library(tsibble)
library(feasts)

ggthemr::ggthemr(palette = "greyscale", line_weight = 0.5, layout = "clean",type = "outer")

redwood <- "#B23A48"
delft <- "#35415A"
cool_gray <- "#9197AE"
orange <- "#FF7F11"
harvest <- "#CC8500"
sea <- "#388659"

#' load in data
df <- read_csv("./data/merged_debt.csv") |>
  rename(iso = countrycode,
         ifs = ifscode)
#' institutional data
defacto <- readxl::read_xlsx("./data/defacto_regimes.xlsx")
dejure <- readxl::read_xlsx("./data/dejure_regimes.xlsx")
wb_classes <- read_csv("./data/wb_classes.csv") %>% janitor::clean_names()

#' find out which countries have the most missing debt data
missing_debt <-
  df |>
  summarize(missing_obs = sum(is.na(gg)),
            .by = c(country, iso, ifs))

tbl <-
  df |>
  mutate(
    growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
    l_growth = log(rgdpo / lag(rgdpo)),
    .by = country
  ) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg)))

missing_debt <-
  tbl |>
  summarize(
    missing_obs_gg = sum(is.na(gg)),
    missing_obs_cg = sum(is.na(cg)),
    missing_obs_debt = sum(is.na(debt)),
    .by = c(country, iso)
  )

excluded <- filter(missing_debt, missing_obs_gg > 20 , missing_obs_cg > 30, !country %in% c("China", "Peru", "Ecuador", "Phillipines", "Venezuela"))

excluded_countries <- unique(excluded$country)

data_df <- filter(tbl, !country %in% c(excluded_countries)) |>
  mutate(
    growth = (gdp - lag(gdp)) / lag(gdp),
    l_growth = log(gdp / lag(gdp)),
    .by = country
  ) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg))) |>
  mutate(
    debt_imp = imputeTS::na_kalman(debt, model = "auto.arima"),
    growth_imp = imputeTS::na_interpolation(growth),
    .by = country
  ) |>
  left_join(wb_classes, by = join_by(iso == code)) |>
  relocate(c(region, income_group, lending_category), .after = country)

included_countries <- distinct(data_df, country,) %>% left_join(defacto) %>% filter(year >= 2015) %>% distinct(country)

data_df |>
  select(year, country, income_group, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  filter(!is.na(income_group)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", "red"), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  facet_wrap(~ income_group, scales = "free")


data_df |>
  as_tsibble(index = year,
             key = c(ifs, iso, country)) -> ts

ts |>
  ggplot(aes(x = year, y = growth_imp, group = country)) +
  geom_line() +
  scale_y_continuous() +
  facet_wrap(~region, scales = "free")

ts_decomp <- ts |>
  # Fit a non-seasonal STL decomposition
  model(
    stl = STL(growth_imp ~ season(period = 1), robust = TRUE)
  ) |>
  components()

outliers <- ts_decomp |>
  mutate(
    lower = quantile(remainder, 0.25) - 3 * IQR(remainder),
    upper = quantile(remainder, 0.75) + 3 * IQR(remainder),
    .by = country
  ) |>
  filter(remainder < lower |
           remainder > upper) |>
  transmute(country, year, outlier = TRUE)

ts |>
  anti_join(outliers) |>
  fill_gaps() -> ts_df
  
ts_df %>%
  ggplot(aes(x = year, y = growth_imp, group = country)) +
  geom_line() +
  scale_y_continuous() +
  facet_wrap( ~ region, scales = "free")

chudik <- read_csv("./data/chudik_countries.csv") %>% janitor::clean_names()
chudik_countries <- unique(chudik$country)

chudik_df <- filter(data_df, country %in% chudik_countries)

distinct(chudik_df, country, iso) -> nations


maddison <- haven::read_dta("./data/mpd2020.dta") %>%
  select(year, iso = countrycode, rgdpc = gdppc, population = pop)

maddison_countries <- distinct(maddison, iso) 

distinct(data_df, country, iso) -> final_countries

inner_join(final_countries, maddison_countries) %>% 
  select(iso) %>%
  as_vector() -> matching_countries

countrycodes <- countrycodes |>
  mutate(country = if_else(country == "Korea" & countrycode == "KOR", "South Korea", as.character(country)))

countrycodes <- countrycodes |>
  mutate(country = if_else(countrycode == "PRK", "North Korea", as.character(country)))

nations |>
  janitor::clean_names() |>
  left_join(maddison) |>
  filter(year >= 1950) |>
  transmute(year,
            country, 
            iso, 
            rgdpc,
            population = population * 1000) |>
  mutate(growth =  (rgdpc - lag(rgdpc)) / lag(rgdpc),
         log_growth  = log(rgdpc / lag(rgdpc)), .by = country) -> chudik_gdp

distinct(chudik_gdp, country, iso) |>
  mutate(has_gdp = TRUE) -> has_growth

chudik |>
  left_join(has_growth) |>
  filter(is.na(has_gdp))

chudik_df |>
  select(year, country, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", "red"), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "")
  
chudik_df |>
  select(year, country, growth, growth_imp, growth_arima) |>
  pivot_longer(cols = c(growth, growth_imp, growth_arima)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  ggsci::scale_colour_npg(labels = c("Original", "Imputed", "ARIMA Imputation")) +
  labs(x = "Debt", y = "Density", color = "")

columns_to_drop <-
  c(
    "gdp",
    "cgdpe",
    "cgdpo",
    "rgdpe",
    "rgdpo",
    "growth",
    "l_growth",
    "growth_imp",
    "rconna",
    "economy"
  )

chudik_data <-
  select(chudik_df, -any_of(columns_to_drop)) |> left_join(maddison) |>
  mutate(
    growth =  (rgdpc - lag(rgdpc)) / lag(rgdpc),
    log_growth  = log(rgdpc / lag(rgdpc)),
    .by = country
  )

full_data <-
  select(data_df, -any_of(columns_to_drop)) |> left_join(maddison) |>
  mutate(
    growth =  (rgdpc - lag(rgdpc)) / lag(rgdpc),
    log_growth  = log(rgdpc / lag(rgdpc)),
    .by = country
  )

final_selected_countries <- full_data  %>% distinct(country) %>%  pull(country)

excluded_countries_classifications <- tibble(country = excluded_countries)  %>% left_join(countrycodes) %>% left_join(wb_classes, join_by(countrycode == code))


write_csv(full_data, "full_final_dataset.csv")
write_csv(chudik_data, "chudik_final_dataset.csv")

chudik_data |>
  mutate(
    growth_imp = imputeTS::na_kalman(growth),.by = country
  )  |> 
  filter(iso %in% matching_countries) |>
  select(year, country, region, growth, growth_imp) |>
  pivot_longer(cols = c(growth, growth_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", saffron), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  theme_clean()

full_data |>
  filter(iso %in% matching_countries) |>
  mutate(
    growth_imp = imputeTS::na_kalman(growth),.by = country
  )  |> 
  select(year, country, region, growth, growth_imp) |>
  pivot_longer(cols = c(growth, growth_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", "red"), labels = c("Original", "Imputed")) +
  labs(x = "Growth", y = "Density", color = "") 


ts <- zoo::zooreg(data = chudik_df, start = 1950, end = 2019)

models <- ARDL::auto_ardl(growth ~ debt_imp, data = ts, max_order = 5)
models$best_order
models$best_model


wb_data_new <- read_csv("data/wb_data_new.csv") |> janitor::clean_names() |> filter(!is.na(country_code))

distinct(wb_data_new, series_name, series_code) -> wb_vars

wb_vars |>
  mutate(variable_name = case_when(
    series_code == "FS.AST.DOMS.GD.ZS" ~ "domestic_credit",
    series_code == "NE.EXP.GNFS.ZS" ~ "exports",
    series_code == "DT.DOD.DECT.CD" ~ "external_debt_total",
    series_code == "BX.KLT.DINV.CD.WD" ~ "fdi_net_inflows",
    series_code == "NY.GDP.MKTP.CD" ~ "gdp_current",
    series_code == "NY.GDP.MKTP.KD.ZG" ~ "gdp_growth_annual",
    series_code == "NY.GNP.PCAP.CD" ~ "gni_per_capita_atlas",
    series_code == "NY.GNP.PCAP.PP.CD" ~ "gni_per_capita_ppp",
    series_code == "NY.GNP.ATLS.CD" ~ "gni_atlas_usd",
    series_code == "NY.GNP.MKTP.PP.CD" ~ "gni_ppp",
    series_code == "NE.GDI.TOTL.ZS" ~ "capital_formation",
    series_code == "NE.IMP.GNFS.ZS" ~ "imports",
    series_code == "SI.DST.FRST.20" ~ "income_share_lowest20",
    series_code == "NV.IND.TOTL.ZS" ~ "industry_value_added_gdp",
    series_code == "NY.GDP.DEFL.KD.ZG" ~ "inflation",
    series_code == "SP.DYN.LE00.IN" ~ "life_expectancy_birth",
    series_code == "TG.VAL.TOTL.GD.ZS" ~ "merchandise_trade",
    series_code == "MS.MIL.XPND.GD.ZS" ~ "military_expenditure",
    series_code == "SP.POP.GROW" ~ "population_growth_annual",
    series_code == "SP.POP.TOTL" ~ "population_total",
    series_code == "GC.TAX.TOTL.GD.ZS" ~ "tax_revenue",
    series_code == "DT.TDS.DECT.EX.ZS" ~ "total_debt_service_exports",
    series_code == "FM.LBL.BMNY.GD.ZS" ~ "broad_money",
    series_code == "GC.DOD.TOTL.GD.ZS" ~ "cg_debt",
    TRUE ~ NA_character_
  )) -> wb_vars

wb_data <-
  wb_data_new |>
  pivot_longer(names_to = "year",
               cols = 5:67) |>
  left_join(wb_vars) |>
  select(-c(series_name, series_code)) |>
  pivot_wider(names_from = variable_name) |>
  mutate(year = seq(1960, 2022, by = 1), .by = country_code) %>%
  mutate(country_name = 
           case_when(country_name == "Egypt, Arab Rep." ~ "Egypt",
                     country_name == "Gambia, The" ~ "Gambia",
                     country_name == "Hong Kong SAR, China" ~ "Hong Kong",
                     country_name == "Korea, Rep." ~ "South Korea",
                     country_name == "Iran, Islamic Rep." ~ "Iran",
                     country_name == "Lao PDR" ~ "Laos",
                     country_name == "Turkiye" ~ "Turkey",
                     country_name == "Venezuela, RB" ~ "Venezuela",
                     TRUE ~ country_name)) |> 
  rename(country = country_name) |> 
  mutate(country = gsub("(.*),.*", "\\1", country)) |>
  mutate(
    country = case_when(
      country == "Hong Kong SAR"          ~ "Hong Kong",
      country == "Kyrgyz Republic"        ~ "Kyrgyzstan",
      country == "Lao PDR"                ~ "Laos",
      country == "Macao SAR"              ~ "Macao",
      country == "North Macedonia"        ~ "Macedonia",
      country == "Slovak Republic"        ~ "Slovakia",
      country == "Syrian Arab Republic"   ~ "Syria",
      country == "St. Kitts and Nevis"    ~ "Saint Kitts and Nevis",
      country == "St. Lucia"              ~ "Saint Lucia",
      country == "USA"                    ~ "United States",
      country == "Viet Nam"               ~ "Vietnam",
      TRUE ~ as.character(country)
    ),
  ) |>
  mutate(country = trimws(country)) |>
  mutate(
    country = case_when(
      country_code == "COD" ~ "Congo D.R.",
      country_code == "KOR" ~ "South Korea",
      TRUE ~ as.character(country)
    )
  )


world_bank_data <-
  wb_data %>%
  select(year,
         country = country_name,
         imports,
         exports,
         population_total)
  
wb_data |>
  mutate(across(
    c(
      domestic_credit,
      exports,
      imports,
      gdp_growth_annual,
      cg_debt,
      broad_money,
      tax_revenue,
      military_expenditure,
      merchandise_trade,
      inflation,
      total_debt_service_exports,
      population_growth_annual,
      income_share_lowest20,
      capital_formation,
      industry_value_added_gdp
    ),
    ~ .x / 100
  )) -> wb_data


fit <-
  lm(
    gdp_growth ~ gg + inflation + labsh + independence_dummy + us_pegger_dummy + eur_pegger_dummy + dual_market_dummy + european_union + exports + imports + exports *
      imports,
    weights = population,
    data = defacto
  )

estimatr::lm_robust(
  growth ~ lag(growth) + debt_imp + lag(debt_imp) + lag(debt_imp, 2) + pl_x + pl_m,
  fixed_effects = ~ country + year,
  data = chudik_data
)
