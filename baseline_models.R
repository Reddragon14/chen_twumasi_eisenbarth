library(tidyverse)
library(estimatr)
library(broom)

final <- read_csv("updated_dataset.csv")

# Specifications ----------------------------------------------------------

responses <- c("p90p100", "p99p100", "gini")

income_groups <- c(
  "High income",
  "Upper middle income",
  "Lower middle income",
  "Low income"
)

specs <- list(
  "(1) Baseline" = c(
    "public_debt", "growth", "life_exp",
    "public_debt:growth"
  ),
  "(2) Fiscal size" = c(
    "public_debt", "growth", "life_exp", "csh_g", "inflation",
    "public_debt:growth"
  ),
  "(3) Mil vs civ" = c(
    "public_debt", "growth", "life_exp", "military_exp", "nonmil_g",
    "composite_inflation", "public_debt:growth"
  ),
  "(4) Debt x civ" = c(
    "public_debt", "growth", "life_exp", "military_exp", "nonmil_g",
    "composite_inflation", "public_debt:growth", "public_debt:nonmil_g"
  ),
  "(5) Full" = c(
    "public_debt", "growth", "life_exp", "military_exp", "nonmil_g",
    "composite_inflation", "trade_openness", "csh_i",
    "public_debt:growth", "public_debt:nonmil_g"
  )
)

fit_fe <- function(dv, rhs, data) {
  lm_robust(
    reformulate(rhs, response = dv),
    data = data,
    fixed_effects = ~ country + year,
    clusters = country,
    se_type = "stata"
  )
}

collect <- function(data) {
  data |>
    mutate(
      n_obs = map_int(fit, \(f) f$nobs),
      tidied = map(fit, \(f) tidy(f, conf.int = TRUE))
    ) |>
    unnest(tidied)
}

# Pooled ------------------------------------------------------------------

pooled <- expand_grid(dv = responses, model = names(specs)) |>
  mutate(fit = map2(dv, model, \(d, m) fit_fe(d, specs[[m]], final)))

collect(pooled) |>
  select(dv, model, term, estimate, std.error, statistic, p.value,
         conf.low, conf.high, df, n_obs) |>
  write_csv("regression_coefficients.csv")

walk2(pooled$fit, paste(pooled$dv, pooled$model), \(fit, label) {
  cat("\n---", label, "---\n")
  print(summary(fit))
})

# Income group splits on specification (3) --------------------------------

by_group <- expand_grid(dv = responses, income_group = income_groups) |>
  mutate(
    fit = map2(dv, income_group, \(d, g) {
      fit_fe(d, specs[["(3) Mil vs civ"]], filter(final, income_group == g))
    })
  )

collect(by_group) |>
  select(dv, income_group, term, estimate, std.error, statistic, p.value,
         conf.low, conf.high, df, n_obs) |>
  write_csv("regression_coefficients_by_group.csv")

walk2(by_group$fit, paste(by_group$dv, by_group$income_group), \(fit, label) {
  cat("\n---", label, "---\n")
  print(summary(fit))
})

# First differences -------------------------------------------------------

diff_vars <- c(
  responses,
  "public_debt", "growth", "life_exp", "military_exp", "nonmil_g", "inflation"
)

est_diff <- final |>
  arrange(country, year) |>
  mutate(
    across(all_of(diff_vars), \(x) x - dplyr::lag(x), .names = "d_{.col}"),
    .by = country
  )

fd <- tibble(dv = paste0("d_", responses)) |>
  mutate(
    fit = map(dv, \(d) {
      lm_robust(
        reformulate(
          c("d_public_debt", "d_growth", "d_life_exp",
            "d_military_exp", "d_nonmil_g", "d_inflation"),
          response = d
        ),
        data = est_diff,
        fixed_effects = ~year,
        clusters = country,
        se_type = "stata"
      )
    })
  )

collect(fd) |>
  select(dv, term, estimate, std.error, statistic, p.value,
         conf.low, conf.high, df, n_obs) |>
  write_csv("regression_coefficients_fd.csv")

walk2(fd$fit, fd$dv, \(fit, label) {
  cat("\n---", label, "---\n")
  print(summary(fit))
})

# Debt coefficient across all specifications and responses ----------------

collect(pooled) |>
  filter(term == "public_debt") |>
  select(dv, model, estimate, std.error, p.value, n_obs) |>
  pivot_wider(
    names_from = dv,
    values_from = c(estimate, std.error, p.value, n_obs)
  )
