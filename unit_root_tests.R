library(tidyverse)
library(tseries)

final <- read_csv("updated_dataset.csv")

test_vars <- c(
  "p90p100",
  "p99p100",
  "public_debt",
  "growth"
)

run_unit_root <- function(x, test = c("adf", "pp")) {
  test <- match.arg(test)
  x <- x[!is.na(x)]
  if (length(x) < 10) {
    return(tibble(statistic = NA_real_, p_value = NA_real_))
  }
  tryCatch(
    {
      res <- switch(test,
                    adf = tseries::adf.test(x, alternative = "stationary"),
                    pp = tseries::pp.test(x, alternative = "stationary")
      )
      tibble(statistic = as.numeric(res$statistic), p_value = res$p.value)
    },
    error = \(e) tibble(statistic = NA_real_, p_value = NA_real_)
  )
}

# Prepare levels and first differences
panel_ur <- final |>
  arrange(iso, year) |>
  mutate(
    log_debt = log(public_debt),
    log_gdp_pc = log(gdp_pc),
    .by = iso
  )

# Run both tests on levels and first differences for each country × variable
ur_results <- panel_ur |>
  select(iso, country, income_group, year, all_of(test_vars)) |>
  pivot_longer(
    cols = all_of(test_vars),
    names_to = "variable",
    values_to = "level"
  ) |>
  arrange(iso, variable, year) |>
  mutate(
    first_diff = level - dplyr::lag(level),
    .by = c(iso, variable)
  ) |>
  nest(.by = c(iso, country, income_group, variable)) |>
  mutate(
    adf_level = map(data, \(d) run_unit_root(d$level, "adf")),
    pp_level = map(data, \(d) run_unit_root(d$level, "pp")),
    adf_fd = map(data, \(d) run_unit_root(d$first_diff, "adf")),
    pp_fd = map(data, \(d) run_unit_root(d$first_diff, "pp")),
    n_obs = map_int(data, \(d) sum(!is.na(d$level)))
  ) |>
  select(-data) |>
  unnest_wider(adf_level, names_sep = "_") |>
  unnest_wider(pp_level, names_sep = "_") |>
  unnest_wider(adf_fd, names_sep = "_") |>
  unnest_wider(pp_fd, names_sep = "_") |>
  rename(
    adf_level_stat = adf_level_statistic,
    adf_level_p = adf_level_p_value,
    pp_level_stat = pp_level_statistic,
    pp_level_p = pp_level_p_value,
    adf_fd_stat = adf_fd_statistic,
    adf_fd_p = adf_fd_p_value,
    pp_fd_stat = pp_fd_statistic,
    pp_fd_p = pp_fd_p_value
  )

#### CLASSIFY ORDER OF INTEGRATION ####

ALPHA <- 0.05

ur_classified <- ur_results |>
  mutate(
    reject_level_adf = adf_level_p < ALPHA,
    reject_level_pp = pp_level_p < ALPHA,
    reject_fd_adf = adf_fd_p < ALPHA,
    reject_fd_pp = pp_fd_p < ALPHA,
    
    # Conservative: require BOTH tests to agree
    stationary_level = reject_level_adf & reject_level_pp,
    stationary_fd = reject_fd_adf | reject_fd_pp,
    integration = case_when(
      stationary_level ~ "I(0)",
      stationary_fd ~ "I(1)",
      .default = "I(2)+"
    ),
    result_level = if_else(stationary_level, "Pass", "Fail"),
    result_fd = if_else(stationary_fd, "Pass", "Fail")
  )

#### VARIABLE-LEVEL PASS/FAIL SUMMARY ####

variable_summary <- ur_classified |>
  summarize(
    n_countries = n(),
    n_i0 = sum(integration == "I(0)"),
    n_i1 = sum(integration == "I(1)"),
    n_i2 = sum(integration == "I(2)+"),
    .by = variable
  ) |>
  mutate(
    pct_i0 = round(100 * n_i0 / n_countries, 1),
    pct_i1 = round(100 * n_i1 / n_countries, 1),
    pct_i2 = round(100 * n_i2 / n_countries, 1),
    modal_order = case_when(
      n_i0 >= n_i1 & n_i0 >= n_i2 ~ "I(0)",
      n_i1 >= n_i2 ~ "I(1)",
      .default = "I(2)+"
    )
  )

cat("\n=== VARIABLE-LEVEL SUMMARY: PASS/FAIL AND ORDER OF INTEGRATION ===\n")
print(variable_summary)

# Countries with I(2)+ for any variable (potential problems)
i2_countries <- ur_classified |>
  filter(integration == "I(2)+") |>
  distinct(iso, country, variable)

if (nrow(i2_countries) > 0) {
  cat("\n=== WARNING: I(2)+ SERIES ===\n")
  i2_countries |>
    arrange(variable, country)
}

#### DETAILED COUNTRY-LEVEL RESULTS, ALL CORE VARIABLES ####

walk(test_vars, \(v) {
  cat("\n=== DETAILED:", v, "===\n")
  ur_classified |>
    filter(variable == v) |>
    select(
      iso, country, integration, result_level, result_fd,
      adf_level_stat, adf_level_p,
      pp_level_stat, pp_level_p,
      adf_fd_stat, adf_fd_p,
      pp_fd_stat, pp_fd_p
    ) |>
    mutate(across(where(is.numeric), ~ round(.x, 3))) |>
    print(n = 120)
})

# ARDL/bounds approach is valid when variables are I(0), I(1), or mixed
# but NOT I(2). Flag countries where any core variable is I(2)+

core_vars <- c("p90p100",
               "p99p100",
               "public_debt",
               "growth")

ardl_valid <- ur_classified |>
  filter(variable %in% core_vars) |>
  summarize(
    max_integration = max(case_when(
      integration == "I(0)" ~ 0L,
      integration == "I(1)" ~ 1L,
      .default = 2L
    )),
    vars_i0 = sum(integration == "I(0)"),
    vars_i1 = sum(integration == "I(1)"),
    vars_i2 = sum(integration == "I(2)+"),
    .by = c(iso, country)
  ) |>
  mutate(
    ardl_valid = max_integration <= 1,
    mix = paste0(vars_i0, " I(0), ", vars_i1, " I(1), ", vars_i2, " I(2)+")
  )

cat("\n=== ARDL VALIDITY CHECK (core variables) ===\n")
cat("Valid (all I(0) or I(1)):", sum(ardl_valid$ardl_valid), "\n")
cat("Invalid (contains I(2)+):", sum(!ardl_valid$ardl_valid), "\n")

write_csv(ur_classified, "./data/unit_root_tests.csv")
write_csv(ardl_valid, "./data/ardl_integration_validity.csv")
write_csv(variable_summary, "./data/variable_integration_summary.csv")