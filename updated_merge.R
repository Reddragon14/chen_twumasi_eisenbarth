library(tidyverse)
library(haven)
library(readxl)
library(zoo)

# Parameters --------------------------------------------------------------

year_start <- 1950
year_end <- 2024

window_start <- 1978
window_end <- 2019

miss_thresh <- 0.20
max_run <- 2

min_distinct <- 20
max_flat_share <- 0.5

max_gap <- 4
max_miss <- 0.20

gate_vars <- c("p90p100", "p99p100", "public_debt", "growth")

impute_vars <- c(
  "public_debt",
  "growth",
  "gdp_pc",
  "govt_expenditure",
  "p90p100",
  "p99p100",
  "gini_mean",
  "trade_openness",
  "composite_inflation",
  "weo_cpi_inflation",
  "military_exp",
  "life_exp",
  "csh_g"
)

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

ggd_series <- c(
  "Debt instruments, General government, Percent of GDP" = "ggd_gg_debt",
  "Debt instruments, Central government, Percent of GDP" = "ggd_cg_debt",
  "Debt instruments, Private sector, Percent of GDP" = "ggd_private_debt",
  "Debt instruments, Households, Percent of GDP" = "ggd_hh_debt",
  "Debt instruments, Public sector, Percent of GDP" = "ggd_public_debt",
  "Debt instruments, Non-financial corporations, Percent of GDP" =
    "ggd_nfc_debt"
)

weo_series <- c(
  "weo_real_gdp_growth",
  "weo_gross_debt",
  "weo_cpi_inflation",
  "weo_gdp_ppp",
  "weo_gdp_pc_ppp",
  "weo_govt_exp_pct_gdp"
) |>
  set_names(c(
    "Gross domestic product (GDP), Constant prices, Percent change",
    "Gross debt, General government, Percent of GDP",
    "All Items, Consumer price index (CPI), Period average, percent change",
    paste0(
      "Gross domestic product (GDP), Current prices, Purchasing power ",
      "parity (PPP) international dollar, ICP benchmarks 2017-2021"
    ),
    paste0(
      "Gross domestic product (GDP), Per capita, purchasing power parity ",
      "(PPP) international dollar, ICP benchmarks 2017-2021"
    ),
    "Expenditure, General government, Percent of GDP"
  ))

wb_series <- c(
  "MS.MIL.XPND.GD.ZS" = "military_exp",
  "NY.GDP.DEFL.KD.ZG" = "inflation",
  "SP.DYN.LE00.IN" = "life_exp",
  "NE.GDI.TOTL.ZS" = "capital_formation",
  "NV.IND.TOTL.ZS" = "industry_va",
  "FS.AST.DOMS.GD.ZS" = "domestic_credit",
  "FM.LBL.BMNY.GD.ZS" = "broad_money"
)

# Helpers -----------------------------------------------------------------

read_wid <- function(path, value_name) {
  read_excel(path) |>
    mutate(country = str_trim(country), year = as.integer(year)) |>
    select(country, year, all_of(value_name))
}

read_imf <- function(data, series) {
  data |>
    filter(indicator %in% names(series)) |>
    mutate(
      country_code = str_extract(series_code, "^[A-Z]{3}"),
      indicator = series[indicator]
    ) |>
    filter(!is.na(country_code)) |>
    select(country_code, indicator, starts_with("x")) |>
    pivot_longer(starts_with("x"), names_to = "year") |>
    mutate(year = as.integer(str_remove(year, "x"))) |>
    filter(year <= year_end) |>
    pivot_wider(names_from = indicator, values_from = value) |>
    arrange(country_code, year)
}

max_run_na <- function(x) {
  runs <- rle(is.na(x))
  max(c(0, runs$lengths[runs$values]))
}

window_qualify <- function(data, start, length, vars = gate_vars) {
  data |>
    filter(!is.na(iso), year >= start, year <= start + length - 1) |>
    complete(iso, year = start:(start + length - 1)) |>
    arrange(iso, year) |>
    summarize(
      across(
        all_of(vars),
        list(gap = max_run_na, miss = \(x) mean(is.na(x)))
      ),
      .by = iso
    ) |>
    filter(
      if_all(ends_with("_gap"), \(x) x <= max_run),
      if_all(ends_with("_miss"), \(x) x <= miss_thresh)
    ) |>
    pull(iso)
}

impute_interior <- function(x) {
  if (max_run_na(x) > max_gap || mean(is.na(x)) > max_miss) {
    return(x)
  }
  imputeTS::na_kalman(x)
}

# Imports -----------------------------------------------------------------

pwt_raw <- read_dta("./data/pwt110.dta")

ggd_raw <- read_csv(
  paste0(
    "./data/dataset_2026-03-29T04_23_18.493273702Z_",
    "DEFAULT_INTEGRATION_IMF.FAD_GDD_2.0.0.csv"
  )
) |>
  janitor::clean_names()

weo_raw <- read_csv(
  paste0(
    "./data/dataset_2026-03-29T04_18_06.872068379Z_",
    "DEFAULT_INTEGRATION_IMF.RES_WEO_9.0.0.csv"
  )
) |>
  janitor::clean_names()

gini_raw <- read_dta("./data/swiid9_91.dta") |>
  select(country, year, 4:103)

wb_raw <- read_csv("./data/wb_data_new.csv") |>
  janitor::clean_names() |>
  filter(!is.na(country_code))

wb_classes_raw <- read_csv("./data/who_analytical_history.csv")

wb_regions <- read_csv("./data/wb_classes.csv") |>
  janitor::clean_names() |>
  select(-c(economy, income_group))

# World Bank income classifications ---------------------------------------

wb_classes <- wb_classes_raw |>
  pivot_longer(
    cols = -c(country_code, country),
    names_to = "year",
    values_to = "class"
  ) |>
  filter(class %in% c("L", "LM", "LM*", "UM", "H")) |>
  mutate(
    income_group = case_when(
      class == "L" ~ "Low income",
      class %in% c("LM", "LM*") ~ "Lower middle income",
      class == "UM" ~ "Upper middle income",
      class == "H" ~ "High income"
    )
  ) |>
  count(country_code, country, income_group) |>
  slice_max(n, n = 1, by = country_code, with_ties = FALSE) |>
  select(code = country_code, income_group)


# Penn World Tables -------------------------------------------------------

pwt <- pwt_raw |>
  mutate(
    country = str_trim(country),
    country = case_when(
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
      country %in% c("Republic of Korea", "Korea") ~ "South Korea",
      country == "Republic of Moldova" ~ "Moldova",
      country == "St. Vincent and the Grenadines" ~
        "Saint Vincent and the Grenadines",
      country == "State of Palestine" ~ "Palestine",
      country == "Türkiye" ~ "Turkey",
      country == "U.R. of Tanzania: Mainland" ~ "Tanzania",
      country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
      country == "Viet Nam" ~ "Vietnam",
      .default = country
    )
  ) |>
  select(iso = countrycode, country, year, all_of(pwt_vars)) |>
  arrange(iso, year) |>
  mutate(
    pwt_growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
    trade_openness = abs(csh_x) + abs(csh_m),
    pwt_gdp_pc = rgdpo / pop,
    .by = iso
  )

# IMF Global Debt Database ------------------------------------------------

ggd <- read_imf(ggd_raw, ggd_series) |>
  mutate(across(starts_with("ggd_"), \(x) x / 100))

# IMF World Economic Outlook ----------------------------------------------

weo <- read_imf(weo_raw, weo_series) |> 
  mutate(
    across(
      c(
        weo_real_gdp_growth,
        weo_gross_debt,
        weo_cpi_inflation,
        weo_govt_exp_pct_gdp
      ),
      \(x) x / 100
    )
  )

# World Inequality Database -----------------------------------------------

wid <- read_wid("./data/WID_Data_29032026-100423.xlsx", "p90p100") |>
  full_join(
    read_wid("./data/WID_Data_29032026-100548.xlsx", "p99p100"),
    by = c("country", "year")
  ) |>
  filter(!str_detect(country, "\\((MER|PPP)\\)$")) |>
  mutate(
    country = case_when(
      country == "Korea" ~ "South Korea",
      country == "Cote d’Ivoire" ~ "Cote d'Ivoire",
      country == "USA" ~ "United States",
      country == "Viet Nam" ~ "Vietnam",
      country %in% c("Rural China", "Urban China") ~ "China",
      .default = country
    )
  ) |>
  summarize(
    across(c(p90p100, p99p100), \(x) mean(x, na.rm = TRUE)),
    .by = c(country, year)
  ) |>
  mutate(across(c(p90p100, p99p100), \(x) na_if(x, NaN)))


# SWIID -------------------------------------------------------------------

gini <- gini_raw |>
  filter(year >= year_start, year <= year_end) |>
  pivot_longer(3:102, names_to = "imp", values_to = "value") |>
  summarize(
    gini_mean = mean(value, na.rm = TRUE),
    gini_sd = sd(value, na.rm = TRUE),
    n_imps = sum(!is.na(value)),
    .by = c(country, year)
  ) |>
  mutate(
    gini_mean = if_else(is.nan(gini_mean) | n_imps == 0, NA_real_, gini_mean),
    gini_sd = if_else(is.nan(gini_sd) | n_imps == 0, NA_real_, gini_sd)
  ) |>
  select(-n_imps) |>
  mutate(
    country = case_when(
      country == "Congo-Kinshasa" ~ "Congo",
      country == "Korea" ~ "South Korea",
      .default = country
    )
  )

# World Development Indicators --------------------------------------------

wb <- wb_raw |>
  filter(series_code %in% names(wb_series)) |>
  mutate(series_name = wb_series[series_code]) |>
  pivot_longer(starts_with("x"), names_to = "year") |>
  mutate(year = as.integer(str_extract(year, "\\d{4}"))) |>
  select(iso = country_code, year, series_name, value) |>
  pivot_wider(names_from = series_name, values_from = value) |>
  mutate(
    across(
      c(
        military_exp,
        inflation,
        capital_formation,
        industry_va,
        domestic_credit,
        broad_money
      ),
      \(x) x / 100
    )
  )

# Merge and composites ----------------------------------------------------

combined <- wid |>
  left_join(pwt, by = c("country", "year")) |> 
  left_join(ggd, join_by(iso == country_code, year)) |>
  left_join(weo, join_by(iso == country_code, year)) |> 
  left_join(gini, by = c("country", "year")) |> 
  left_join(wb, by = c("iso", "year")) |> 
  left_join(wb_classes, join_by(iso == code)) |>
  filter(year >= year_start, year <= year_end) |>
  mutate(
    debt_source = case_when(
      !is.na(ggd_gg_debt) ~ "ggd_gg",
      !is.na(ggd_cg_debt) ~ "ggd_cg",
      !is.na(weo_gross_debt) ~ "weo"
    ),
    public_debt = coalesce(ggd_gg_debt, ggd_cg_debt, weo_gross_debt),
    growth_source = case_when(
      !is.na(pwt_growth) ~ "pwt",
      !is.na(weo_real_gdp_growth) ~ "weo"
    ),
    growth = coalesce(pwt_growth, weo_real_gdp_growth),
    gdp_pc_source = case_when(
      !is.na(pwt_gdp_pc) ~ "pwt",
      !is.na(weo_gdp_pc_ppp) ~ "weo"
    ),
    gdp_pc = coalesce(pwt_gdp_pc, weo_gdp_pc_ppp),
    composite_inflation = coalesce(inflation, weo_cpi_inflation),
    govt_exp_source = case_when(
      !is.na(csh_g) ~ "pwt",
      !is.na(weo_govt_exp_pct_gdp) ~ "weo"
    ),
      govt_expenditure = weo_govt_exp_pct_gdp,
      nonmil_g = weo_govt_exp_pct_gdp - military_exp
  )

write_csv(combined, "initial_data.csv")

# Window search -----------------------------------------------------------

window_search <- expand_grid(
  start = year_start:year_end,
  length = seq(30, 50, by = 5)
) |>
  filter(start + length - 1 <= year_end) |>
  mutate(
    n_countries = map2_int(
      start,
      length,
      \(s, l) length(window_qualify(combined, s, l))
    )
  ) |>
  arrange(desc(n_countries), desc(length), start)

kept <- window_qualify(combined, window_start, window_end - window_start + 1)

# varied <- combined |>
#   filter(year >= window_start, year <= window_end, iso %in% kept) |>
#   arrange(iso, year) |>
#   summarize(
#     n_distinct_p90 = n_distinct(p90p100, na.rm = TRUE),
#     n_distinct_p99 = n_distinct(p99p100, na.rm = TRUE),
#     flat_share_p90 = mean(p90p100 == dplyr::lag(p90p100), na.rm = TRUE),
#     .by = iso
#   )
# 
# write_csv(varied, "variation_report.csv")
# 
# kept <- varied |>
#   filter
#     n_distinct_p90 >= min_distinct,
#     flat_share_p90 <= max_flat_share
#   ) |>
#   pull(iso) |>
#   intersect(kept)

# Imputation --------------------------------------------------------------

combined_filtered <- combined |>
  filter(year >= window_start, year <= window_end, iso %in% kept) |>
  arrange(iso, year) |>
  mutate(
    across(all_of(intersect(impute_vars, names(combined))), impute_interior),
    .by = iso
  ) |>
  mutate(
    nonmil_g = csh_g - military_exp,
    log_gini = log(gini_mean),
    d_p90 = p90p100 - dplyr::lag(p90p100),
    d_p99 = p99p100 - dplyr::lag(p99p100),
    .by = iso
  )

cat("Window:", window_start, "-", window_end, "\n")
cat("Countries:", length(kept), "\n")
cat(
  "Rows:", nrow(combined_filtered),
  "of expected", length(kept) * (window_end - window_start + 1), "\n"
)

# Output ------------------------------------------------------------------

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
    pop = pop * 1e6,
    emp = emp * 1e6,
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

zero_military <- c(
  "Costa Rica", 
  "Iceland", 
  "Dominica",
  "Grenada",
  "Saint Vincent and the Grenadines", 
  "Bahamas", 
  "Barbados",
  "Maldives",
  "Sao Tome and Principe"
)

final <- final |>
  mutate(
    military_exp = if_else(
      country %in% zero_military & is.na(military_exp), 0, military_exp
    ),
    nonmil_g = csh_g - military_exp
  )

cat("Countries:", n_distinct(final$country), "\n")
cat("Observations:", nrow(final), "\n")

write_csv(final, "updated_dataset.csv")
