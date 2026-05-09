library(tidyverse)
library(haven)
library(readxl)
library(zoo)

##### DATA IMPORTS ####

pwt_raw <- read_dta("./data/pwt110.dta")

ggd_raw <- read_csv("./data/dataset_2026-03-29T04_23_18.493273702Z_DEFAULT_INTEGRATION_IMF.FAD_GDD_2.0.0.csv") |>
  janitor::clean_names()

weo_raw <- read_csv("./data/dataset_2026-03-29T04_18_06.872068379Z_DEFAULT_INTEGRATION_IMF.RES_WEO_9.0.0.csv") |>
  janitor::clean_names()

wid_data_p90 <- read_xlsx("./data/WID_Data_29032026-100423.xlsx")
wid_data_p99 <- read_xlsx("./data/WID_Data_29032026-100548.xlsx")

wb_classes <- read_csv("./data/who_analytical_history.csv")

wb_classes <- wb_classes |>
  pivot_longer(cols = -c(country_code, country), names_to = "year", values_to = "class") |>
  filter(class %in% c("L", "LM", "LM*", "UM", "H")) |>
  mutate(income_group = case_when(
    class == "L"              ~ "Low income",
    class %in% c("LM", "LM*") ~ "Lower middle income",
    class == "UM"             ~ "Upper middle income",
    class == "H"              ~ "High income"
  )) |>
  count(country_code, country, income_group) |>
  slice_max(n, n = 1, by = country_code, with_ties = FALSE) |>
  select(code = country_code, income_group)

wb_regions <- read_csv("./data/wb_classes.csv") |> janitor::clean_names() |>
  select(-c(economy, income_group))

gini_raw <- read_dta("./data/swiid9_91.dta") |>
  select(country, year, 4:103)

wb_raw <- read_csv("./data/wb_data_new.csv") |>
  janitor::clean_names() |>
  filter(!is.na(country_code))

YEAR_START <- 1950
YEAR_END <- 2024

pwt_vars <- c(
  "rgdpe",
  "rgdpo",
  "pop",
  "emp",
  "hc",
  "ctfp",
  "labsh",
  "csh_c",
  "csh_i",
  "csh_g",
  "csh_x",
  "csh_m",
  "pl_c",
  "pl_i",
  "pl_g",
  "pl_x",
  "pl_m",
  "xr",
  "ck",
  "cn"
)

pwt <- pwt_raw |>
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "China, Hong Kong SAR" ~ "Hong Kong",
    country == "China, Macao SAR" ~ "Macao",
    country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    country == "Curaçao" ~ "Curacao",
    country == "Czechia" ~ "Czech Republic",
    country == "D.R. of the Congo" ~ "DR Congo",
    country == "Eswatini" ~ "Swaziland",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Lao People's DR" ~ "Lao PDR",
    country == "Republic of Korea" ~ "Korea",
    country == "Republic of Moldova" ~ "Moldova",
    country == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
    country == "State of Palestine" ~ "Palestine",
    country == "Türkiye" ~ "Turkey",
    country == "U.R. of Tanzania: Mainland" ~ "Tanzania",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    .default = country
  )) |>
  select(iso = countrycode, country, year, all_of(pwt_vars)) |>
  arrange(iso, year) |>
  mutate(
    pwt_growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
    trade_openness = abs(csh_x) + abs(csh_m),
    pwt_gdp_pc = rgdpo / pop,
    .by = iso
  )

# GGD
ggd <- ggd_raw |>
  filter(indicator %in% c(
    "Debt instruments, General government, Percent of GDP",
    "Debt instruments, Central government, Percent of GDP",
    "Debt instruments, Private sector, Percent of GDP",
    "Debt instruments, Households, Percent of GDP",
    "Debt instruments, Public sector, Percent of GDP",
    "Debt instruments, Non-financial corporations, Percent of GDP"
  )) |>
  mutate(
    country_code = str_extract(series_code, "^[A-Z]{3}"),
    indicator = case_when(
      indicator == "Debt instruments, General government, Percent of GDP" ~ "ggd_gg_debt",
      indicator == "Debt instruments, Central government, Percent of GDP" ~ "ggd_cg_debt",
      indicator == "Debt instruments, Private sector, Percent of GDP" ~ "ggd_private_debt",
      indicator == "Debt instruments, Households, Percent of GDP" ~ "ggd_hh_debt",
      indicator == "Debt instruments, Public sector, Percent of GDP" ~ "ggd_public_debt",
      indicator == "Debt instruments, Non-financial corporations, Percent of GDP" ~ "ggd_nfc_debt",
    )
  ) |>
  select(country_code, indicator, starts_with("x")) |>
  filter(!is.na(country_code)) |>
  pivot_longer(starts_with("x"), names_to = "year") |>
  mutate(year = str_remove(year, "x") |> as.integer()) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  arrange(country_code, year) |>
  mutate(across(ggd_public_debt:ggd_private_debt, ~ .x / 100))

# WEO
weo <- weo_raw |>
  filter(indicator %in% c(
    "Gross domestic product (GDP), Constant prices, Percent change",
    "Gross debt, General government, Percent of GDP",
    "All Items, Consumer price index (CPI), Period average, percent change",
    "Gross domestic product (GDP), Current prices, Purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021",
    "Gross domestic product (GDP), Per capita, purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021",
    "Expenditure, General government, Percent of GDP"
  )) |>
  mutate(
    country_code = str_extract(series_code, "^[A-Z]{3}"),
    indicator = case_when(
      indicator == "Gross domestic product (GDP), Constant prices, Percent change" ~ "weo_real_gdp_growth",
      indicator == "Gross debt, General government, Percent of GDP" ~ "weo_gross_debt",
      indicator == "All Items, Consumer price index (CPI), Period average, percent change" ~ "weo_cpi_inflation",
      indicator == "Gross domestic product (GDP), Current prices, Purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021" ~ "weo_gdp_ppp",
      indicator == "Gross domestic product (GDP), Per capita, purchasing power parity (PPP) international dollar, ICP benchmarks 2017-2021" ~ "weo_gdp_pc_ppp",
      indicator == "Expenditure, General government, Percent of GDP" ~ "weo_govt_exp_pct_gdp",
    )
  ) |>
  select(country_code, indicator, starts_with("x")) |>
  filter(!is.na(country_code)) |>
  pivot_longer(starts_with("x"), names_to = "year") |>
  mutate(year = str_remove(year, "x") |> as.integer()) |>
  filter(year <= YEAR_END) |>
  pivot_wider(names_from = indicator, values_from = value) |>
  arrange(country_code, year) |>
  mutate(across(
    c(
      "weo_real_gdp_growth",
      "weo_gross_debt",
      "weo_cpi_inflation",
      "weo_govt_exp_pct_gdp"
    ),
    ~ .x / 100
  ))

# WID
wid <- wid_data_p90 |>
  select(country, year, p90p100) |>
  mutate(country = str_trim(country)) |>
  full_join(
    wid_data_p99 |>
      select(country, year, p99p100) |>
      mutate(country = str_trim(country)),
    by = c("country", "year")
  ) |>
  mutate(country = case_when(
    country == "Korea" ~ "South Korea",
    country == "Cote d’Ivoire" ~ "Cote d'Ivoire",
    country == "USA" ~ "United States",
    country == "Viet Nam" ~ "Vietnam",
    .default = country
  ))

wid <- wid |>
  mutate(country = case_when(
    country %in% c("Rural China", "Urban China") ~ "China",
    .default = country
  )) |>
  summarize(
    p90p100 = mean(p90p100, na.rm = TRUE),
    p99p100 = mean(p99p100, na.rm = TRUE),
    .by = c(country, year)
  ) |>
  mutate(across(c(p90p100, p99p100), ~ na_if(.x, NaN)))

wid <- wid |>
  arrange(country, year) |> 
  mutate(
    d_p99 = p99p100 - dplyr::lag(p99p100),
    d_p90 = p90p100 - dplyr::lag(p90p100),
    .by = c(country)
  )

# SWIID GINI
gini <- gini_raw |>
  filter(year >= YEAR_START, year <= YEAR_END) |>
  pivot_longer(3:102, names_to = "imp", values_to = "value") |>
  summarize(
    gini_mean = mean(value, na.rm = TRUE),
    gini_sd = sd(value, na.rm = TRUE),
    n_imps = sum(!is.na(value)),
    .by = c(country, year)
  ) |>
  mutate(
    gini_mean = if_else(is.nan(gini_mean) | n_imps == 0, NA_real_, gini_mean),
    gini_sd   = if_else(is.nan(gini_sd) | n_imps == 0, NA_real_, gini_sd),
  ) |>
  select(-n_imps) |>
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Cote d'Ivoire",
    country == "Congo-Kinshasa" ~ "Congo",
    country == "Korea" ~ "South Korea",
    .default = country
  ))

# WORLD BANK
wb_series <- c(
  "MS.MIL.XPND.GD.ZS" = "military_exp",
  "NY.GDP.DEFL.KD.ZG" = "inflation",
  "SP.DYN.LE00.IN" = "life_exp",
  "NE.GDI.TOTL.ZS" = "capital_formation",
  "NV.IND.TOTL.ZS" = "industry_va",
  "FS.AST.DOMS.GD.ZS" = "domestic_credit",
  "FM.LBL.BMNY.GD.ZS" = "broad_money"
)

wb <- wb_raw |>
  filter(series_code %in% names(wb_series)) |>
  mutate(series_name = wb_series[series_code]) |>
  pivot_longer(starts_with("x"), names_to = "year") |>
  mutate(year = str_extract(year, "\\d{4}") |> as.integer()) |>
  select(iso = country_code, year, series_name, value) |>
  pivot_wider(names_from = series_name, values_from = value) |> 
  # mutate(
  #   country = str_replace(country, "(.*),.*", "\\1") |> trimws(),
  #   country = case_when(
  #     country == "Hong Kong SAR" ~ "Hong Kong",
  #     coumtru == "Korea, Rep." ~ "South Korea",
  #     country == "Korea, Dem. People's Rep." ~ "North Korea",
  #     country == "Kyrgyz Republic" ~ "Kyrgyzstan",
  #     country == "Lao PDR" ~ "Lao PDR",
  #     country == "Macao SAR" ~ "Macao",
  #     country == "North Macedonia" ~ "North Macedonia",
  #     country == "Slovak Republic" ~ "Slovakia",
  #     country == "Syrian Arab Republic" ~ "Syrian Arab Republic",
  #     country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
  #     country == "St. Lucia" ~ "Saint Lucia",
  #     .default = country
  #   )
  # ) |>
  mutate(across(
    c(military_exp, inflation, capital_formation, industry_va, domestic_credit, broad_money),
    ~ .x / 100
  ))

# MERGE
combined <- pwt |>
  left_join(ggd, join_by(iso == country_code, year)) |>
  left_join(weo, join_by(iso == country_code, year)) |>
  left_join(wid, by = c("country", "year")) |>
  left_join(gini, by = c("country", "year")) |>
  left_join(wb, by = c("iso", "year")) |>
  left_join(wb_classes, by = join_by(iso == code)) |>
  filter(year >= YEAR_START, year <= YEAR_END)

# COMPOSITES
combined <- combined |>
  mutate(
    debt_source = case_when(
      !is.na(ggd_gg_debt) ~ "ggd_gg",
      !is.na(ggd_cg_debt) ~ "ggd_cg",
      !is.na(weo_gross_debt) ~ "weo",
    ),
    public_debt = coalesce(ggd_gg_debt, ggd_cg_debt, weo_gross_debt),

    # Growth: PWT to WEO
    growth_source = case_when(
      !is.na(pwt_growth) ~ "pwt",
      !is.na(weo_real_gdp_growth) ~ "weo",
    ),
    growth = coalesce(pwt_growth, weo_real_gdp_growth),

    # GDP per capita: PWT to WEO
    gdp_pc_source = case_when(
      !is.na(pwt_gdp_pc) ~ "pwt",
      !is.na(weo_gdp_pc_ppp) ~ "weo",
    ),
    gdp_pc = coalesce(pwt_gdp_pc, weo_gdp_pc_ppp),
    
    composite_inflation = coalesce(inflation, weo_cpi_inflation),

    # Govt expenditure: PWT csh_g to WEO
    govt_exp_source = case_when(
      !is.na(csh_g) ~ "pwt",
      !is.na(weo_govt_exp_pct_gdp) ~ "weo",
    ),
    govt_expenditure = coalesce(csh_g, weo_govt_exp_pct_gdp),

    # Derived
    nonmil_g = csh_g - military_exp,
    log_gini = log(gini_mean),
  )


# ============================================================
# CONSECUTIVE RUN ANALYSIS (Chudik et al. minimum T = 30)
# ============================================================

MIN_CONSEC <- 30

joint_coverage <- combined |>
  arrange(iso, year) |>
  mutate(
    has_triad = !is.na(public_debt) & !is.na(growth) & !is.na(p90p100),
    run_id = consecutive_id(has_triad),
    .by = iso
  ) |>
  filter(has_triad) |>
  summarize(
    run_length = n(),
    run_start = min(year),
    run_end = max(year),
    .by = c(iso, country, income_group, run_id)
  ) |>
  slice_max(run_length, n = 1, by = iso, with_ties = FALSE)

final_countries <- joint_coverage |>
  filter(run_length >= MIN_CONSEC) |>
  pull(country)

all_countries <- distinct(combined, country, iso)

cat("Countries with >=", MIN_CONSEC, "consecutive years:", length(final_countries), "\n")
excluded_countries <- filter(all_countries, !country %in% final_countries)

joint_coverage |>
  filter(run_length >= MIN_CONSEC) |>
  count(income_group, name = "n_countries") |>
  print()

joint_coverage |>
  filter(run_length >= MIN_CONSEC) |>
  arrange(income_group, country) |>
  select(iso, country, income_group, run_length, run_start, run_end) |>
  print(n = 120)

# IMPUTATION
impute_vars <- c(
  "public_debt",
  "growth",
  "gdp_pc",
  "govt_expenditure",
  "weo_cpi_inflation",
  "p90p100",
  "p99p100",
  "trade_openness",
  "composite_inflation"
)

combined_filtered <- combined |>
#  filter(country %in% chudik_countries) |> 
  semi_join(
    joint_coverage |> filter(run_length >= MIN_CONSEC),
    by = "iso"
  ) |>
  left_join(
    joint_coverage |>
      filter(run_length >= MIN_CONSEC) |>
      select(iso, run_start, run_end),
    by = "iso"
  ) |>
  filter(year >= run_start, year <= run_end) |>
  select(-run_start, -run_end)

for (v in intersect(impute_vars, names(combined_filtered))) {
  combined_filtered <- combined_filtered |>
    mutate(!!v := imputeTS::na_ma(!!sym(v), k = 2), .by = iso)
}

final <- combined_filtered |>
  left_join(wb_regions, join_by(iso == code)) |> 
  transmute(
    year,
    iso,
    country,
    region,
    income_group,
    lending_category,
    public_debt,
    growth = round(growth, 4),
    gdp_pc,
    govt_expenditure,
    p90p100,
    p99p100,
    d_p90,
    d_p99,
    gini = gini_mean,
    gini_sd,
    log_gini,
    ggd_gg_debt,
    ggd_cg_debt,
    ggd_private_debt,
    ggd_hh_debt,
    ggd_public_debt,
    ggd_nfc_debt,
    weo_gross_debt,
    weo_real_gdp_growth,
    weo_cpi_inflation,
    inflation,
    composite_inflation,
    weo_gdp_ppp,
    weo_gdp_pc_ppp,
    weo_govt_exp_pct_gdp,
    rgdpe,
    rgdpo,
    pop = pop * 1E6,
    emp = emp * 1E6,
    hc,
    ctfp,
    labsh,
    csh_c,
    csh_i,
    csh_g,
    csh_x,
    csh_m,
    trade_openness,
    pwt_growth,
    pwt_gdp_pc,
    pl_c,
    pl_i,
    pl_g,
    pl_x,
    pl_m,
    xr,
    ck,
    cn,
    military_exp,
    life_exp,
    capital_formation,
    industry_va,
    domestic_credit,
    broad_money,
    nonmil_g
  ) |>
  arrange(iso, year)

cat("Countries:", n_distinct(final$country), "\n")
cat("Observations:", nrow(final), "\n")

write_csv(final, "updated_dataset.csv")
