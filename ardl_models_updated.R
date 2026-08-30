# Extended ARDL analysis: bounds tests, long-run multipliers, and ECM
# Estimated for each specification, response variable, and country.

library(tidyverse)
library(ARDL)
library(broom)
library(progressr)
library(metafor)
library(paletteer)
library(tidytext)
library(extrafont)

ggthemr::ggthemr(palette = "greyscale", layout = "clean", type = "outer")

custom_theme <- function() {
  theme(
    text = element_text(family = "Arial", colour = "#111111"),
    plot.title = element_text(
      size = 11, face = "plain", colour = "#111111",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_text(
      size = 9, colour = "#555555", lineheight = 1.4,
      margin = margin(b = 14)
    ),
    plot.caption = element_text(
      size = 8, colour = "#888888", hjust = 0,
      margin = margin(t = 10)
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border = element_blank(),
    axis.line = element_line(colour = "#cccccc", linewidth = 0.4),
    axis.title = element_text(size = 9, colour = "#444444"),
    axis.text = element_text(size = 8, colour = "#555555"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    legend.title = element_text(size = 8, colour = "#444444"),
    legend.text = element_text(size = 8, colour = "#555555"),
    plot.margin = margin(16, 20, 12, 16)
  )
}

# Parameters --------------------------------------------------------------

responses <- c("p90p100", "p99p100", "gini")

response_labels <- c(
  p90p100 = "Top 10% share",
  p99p100 = "Top 1% share",
  gini = "Gini"
)

to_pp <- c(p90p100 = 1, p99p100 = 1, gini = 1 / 100)


specs <- list(
  "Baseline (p=3)" = list(
    regressors = c("public_debt", "growth", "life_exp", "gdp_x_debt"),
    max_order = 3
  ),
  "Baseline (p=2)" = list(
    regressors = c("public_debt", "growth", "life_exp", "gdp_x_debt"),
    max_order = 3
  ),
  "Composition" = list(
    regressors = c(
      "public_debt", "growth", "life_exp", "gdp_x_debt",
      "military_exp", "nonmil_g"
    ),
    max_order = 3
  )
)

criterion <- "AIC"

income_levels <- c(
  "Low income", 
  "Lower middle income",
  "Upper middle income",
  "High income"
)

summ_dir <- "./data/ardl_summaries"
if (!dir.exists(summ_dir)) dir.create(summ_dir, recursive = TRUE)

min_rows_for <- function(spec) {
  5 + 1 + spec$max_order + length(spec$regressors) * (1 + spec$max_order)
}

# Data --------------------------------------------------------------------

ardl_valid <- read_csv("./data/ardl_integration_validity.csv")

valid_countries <- ardl_valid |>
  filter(ardl_valid) |>
  pull(country)

all_regressors <- specs |>
  map("regressors") |>
  reduce(union)

work <- read_csv("updated_dataset.csv") |>
  mutate(year = as.integer(year), gdp_x_debt = growth * public_debt) |>
  filter(country %in% valid_countries) |>
  select(
    iso, country, income_group, year,
    all_of(responses), all_of(all_regressors)
  ) |>
  arrange(country, year)

countries <- sort(unique(work$country))

# Coverage of the added regressors, before anything is estimated
work |>
  summarize(
    across(all_of(all_regressors), \(x) mean(!is.na(x))),
    .by = country
  ) |>
  pivot_longer(-country, names_to = "regressor", values_to = "coverage") |>
  summarize(
    median_coverage = median(coverage),
    n_countries_full = sum(coverage == 1),
    n_countries_under_80 = sum(coverage < 0.8),
    .by = regressor
  ) |>
  print()

# Extraction --------------------------------------------------------------

extract_longrun <- function(model, dv, regressors) {
  cf <- coef(model)
  cf_names <- names(cf)
  v <- vcov(model)
  dof <- df.residual(model)
  
  # Aliased coefficients from a rank-deficient fit are NA, and NA
  # propagates through the sums below regardless of which term is
  # affected, so such a model has no identifiable long-run relationship.
  if (anyNA(cf)) {
    return(NULL)
  }
  
  c_vec <- as.numeric(str_detect(cf_names, str_c("^L\\(", dv, ",\\s*\\d+\\)$")))
  sum_phi <- sum(c_vec * cf)
  d <- 1 - sum_phi
  
  if (!is.finite(d) || abs(d) < 1e-8) {
    return(NULL)
  }
  
  map_dfr(regressors, function(xvar) {
    a_vec <- as.numeric(cf_names == xvar) +
      as.numeric(str_detect(cf_names, str_c("^L\\(", xvar, ",\\s*\\d+\\)$")))
    
    n <- sum(a_vec * cf)
    lr <- n / d
    
    grad <- (a_vec * d + n * c_vec) / d^2
    var_lr <- as.numeric(t(grad) %*% v %*% grad)
    se_lr <- if (is.finite(var_lr) && var_lr >= 0) sqrt(var_lr) else NA_real_
    
    tibble(
      variable = xvar,
      sum_beta = n,
      sum_phi = sum_phi,
      longrun_multiplier = lr,
      se = se_lr,
      t_stat = lr / se_lr,
      p_value = 2 * pt(-abs(lr / se_lr), df = dof),
      ci_lo = lr - qt(0.975, dof) * se_lr,
      ci_hi = lr + qt(0.975, dof) * se_lr
    )
  })
}

classify_bounds <- function(p) {
  case_when(
    p <= 0.01 ~ "Cointegration: Strong evidence (p <= 0.01)",
    p <= 0.05 ~ "Cointegration: Evidence (p <= 0.05)",
    p <= 0.10 ~ "Weak evidence of cointegration (0.05 < p <= 0.10)",
    .default = "No evidence of cointegration (p > 0.10)"
  )
}

perform_bounds_test <- function(ardl_model) {
  tryCatch(
    {
      bt_f <- bounds_f_test(ardl_model, case = "uc")
      f_stat <- bt_f$statistic[["F"]]
      f_pvalue <- bt_f$p.value[["p_value.F"]]
      tibble(
        f_statistic = f_stat,
        f_pvalue = f_pvalue,
        conclusion = classify_bounds(f_pvalue),
        case_used = "uc"
      )
    },
    error = function(e) {
      tryCatch(
        {
          bt_f <- bounds_f_test(ardl_model, case = "rc")
          f_stat <- bt_f$statistic[["F"]]
          f_pvalue <- bt_f$p.value[["p_value.F"]]
          tibble(
            f_statistic = f_stat,
            f_pvalue = f_pvalue,
            conclusion = classify_bounds(f_pvalue),
            case_used = "rc"
          )
        },
        error = function(e2) {
          tibble(
            f_statistic = NA_real_,
            f_pvalue = NA_real_,
            conclusion = paste("Error:", conditionMessage(e2)),
            case_used = NA_character_
          )
        }
      )
    }
  )
}

# Estimation --------------------------------------------------------------

fit_country <- function(spec_name, dv, cc) {
  spec <- specs[[spec_name]]
  regressors <- spec$regressors
  
  sub <- work |>
    filter(country == cc) |>
    arrange(year) |>
    drop_na(all_of(c(dv, regressors)))
  
  base <- tibble(
    spec = spec_name,
    dv = dv,
    country = cc,
    income_group = first(work$income_group[work$country == cc]),
    n_obs = nrow(sub)
  )
  
  if (nrow(sub) < min_rows_for(spec)) {
    return(list(
      meta = mutate(base, status = "skipped: too few observations",
                    aic = NA_real_, bic = NA_real_,
                    best_order = NA_character_)
    ))
  }
  
  fit <- tryCatch(
    auto_ardl(
      formula = reformulate(regressors, response = dv),
      data = sub,
      max_order = spec$max_order,
      selection = criterion,
      na.action = na.omit
    ),
    error = \(e) e
  )
  
  if (inherits(fit, "error")) {
    return(list(
      meta = mutate(base, status = str_c("failed: ", conditionMessage(fit)),
                    aic = NA_real_, bic = NA_real_,
                    best_order = NA_character_)
    ))
  }
  
  bm <- fit$best_model
  sm <- summary(bm)
  
  ecm_r2 <- tryCatch(summary(recm(bm, case = 3))$r.squared,
                     error = \(e) NA_real_)
  
  longrun <- extract_longrun(bm, dv, regressors)
  
  list(
    meta = mutate(
      base,   
      status = if (is.null(longrun)) "ok: rank-deficient, long-run undefined" else "ok",
      aic = AIC(bm),
      bic = BIC(bm),
      best_order = str_c("(", str_c(fit$best_order, collapse = ","), ")")
    ),
    coefs = as_tibble(sm$coefficients, rownames = "term") |>
      rename(estimate = Estimate, std_error = `Std. Error`,
             t_value = `t value`, p_value = `Pr(>|t|)`) |>
      mutate(spec = spec_name, dv = dv, country = cc, .before = 1),
    bounds = perform_bounds_test(bm) |>
      mutate(spec = spec_name, dv = dv, country = cc, .before = 1),
    longrun = if (is.null(longrun)) NULL else {
      longrun |>
        mutate(spec = spec_name, dv = dv, country = cc,
               income_group = base$income_group, .before = 1)
    },
    # The speed of adjustment is the negative of the multiplier denominator,
    # so it is read off sum_phi rather than parsed from the recm output.
    ecm = tibble(
      spec = spec_name,
      dv = dv,
      country = cc,
      ect_coefficient = if (is.null(longrun)) NA_real_ else {
        first(longrun$sum_phi) - 1
      },
      ecm_r_squared = ecm_r2
    )
  )
}

grid <- expand_grid(spec = names(specs), dv = responses, country = countries)

with_progress({
  p <- progressor(steps = nrow(grid))
  fits <- pmap(grid, \(spec, dv, country) {
    p(message = str_c(spec, " | ", dv, ": ", country))
    fit_country(spec, dv, country)
  })
})

meta_df <- map_dfr(fits, "meta")
coef_df <- map_dfr(fits, "coefs")
bounds_df <- map_dfr(fits, "bounds")
longrun_df <- map_dfr(fits, "longrun")
ecm_df <- map_dfr(fits, "ecm")

write_csv(meta_df, file.path(summ_dir, "00_meta_summary.csv"))
write_csv(bounds_df, file.path(summ_dir, "01_bounds_tests.csv"))
write_csv(longrun_df, file.path(summ_dir, "02_longrun_multipliers.csv"))
write_csv(ecm_df, file.path(summ_dir, "03_ecm_adjustment.csv"))
write_csv(coef_df, file.path(summ_dir, "06_country_coefficients.csv"))

meta_df |>
  count(spec, dv, status) |>
  pivot_wider(names_from = dv, values_from = n, values_fill = 0) |>
  print(n = Inf)

# Meta-analysis -----------------------------------------------------------

longrun_clean <- longrun_df |>
  filter(is.finite(longrun_multiplier), is.finite(se), se > 0, sum_phi < 1) |>
  mutate(
    income_group = factor(income_group, levels = income_levels),
    multiplier_pp = longrun_multiplier * to_pp[dv]
  )

pool <- function(data) {
  m <- rma(yi = longrun_multiplier, sei = se, data = data, method = "REML")
  tibble(
    k = m$k, estimate = m$beta[1], se = m$se, z = m$zval, p_value = m$pval,
    ci_lo = m$ci.lb, ci_hi = m$ci.ub, tau2 = m$tau2, i2 = m$I2,
    q = m$QE, q_p = m$QEp
  )
}

meta_longrun <- longrun_clean |>
  nest(.by = c(spec, dv, variable)) |>
  mutate(pooled = map(data, pool)) |>
  select(-data) |>
  unnest(pooled) |>
  mutate(estimate_pp = estimate * to_pp[dv]) |>
  arrange(spec, dv, variable)

write_csv(meta_longrun, file.path(summ_dir, "04_meta_longrun_summary.csv"))

debt_lr <- filter(longrun_clean, variable == "public_debt")

omnibus <- debt_lr |>
  nest(.by = c(spec, dv)) |>
  mutate(
    fit = map(data, \(d) {
      rma(yi = longrun_multiplier, sei = se, mods = ~income_group,
          data = d, method = "REML")
    }),
    qm = map_dbl(fit, "QM"),
    qm_df = map_dbl(fit, \(f) f$m - 1),
    qm_p = map_dbl(fit, "QMp"),
    r2 = map_dbl(fit, \(f) if (is.null(f$R2)) NA_real_ else f$R2)
  ) |>
  select(spec, dv, qm, qm_df, qm_p, r2)

write_csv(omnibus, file.path(summ_dir, "07_meta_omnibus.csv"))
print(omnibus)

by_group <- debt_lr |>
  nest(.by = c(spec, dv)) |>
  mutate(
    tidied = map(data, \(d) {
      rma(yi = longrun_multiplier, sei = se, mods = ~ income_group - 1,
          data = d, method = "REML") |>
        summary() |>
        coef() |>
        as_tibble(rownames = "income_group") |>
        mutate(income_group = str_remove(income_group, "^income_group"))
    })
  ) |>
  select(-data) |>
  unnest(tidied) |>
  mutate(estimate_pp = estimate * to_pp[dv])

write_csv(by_group, file.path(summ_dir, "05_meta_regression_income_group.csv"))
print(by_group, n = Inf)

# Subsample sensitivity ---------------------------------------------------

sensitivity <- bind_rows(
  debt_lr |>
    nest(.by = c(spec, dv)) |>
    mutate(subsample = "All countries"),
  debt_lr |>
    semi_join(
      filter(bounds_df, str_detect(conclusion, "^Cointegration:|^Weak")),
      by = c("spec", "dv", "country")
    ) |>
    nest(.by = c(spec, dv)) |>
    mutate(subsample = "Cointegrated, p <= 0.10"),
  debt_lr |>
    semi_join(
      filter(bounds_df, str_starts(conclusion, "Cointegration:")),
      by = c("spec", "dv", "country")
    ) |>
    nest(.by = c(spec, dv)) |>
    mutate(subsample = "Cointegrated, p <= 0.05")
) |>
  mutate(pooled = map(data, pool)) |>
  select(-data) |>
  unnest(pooled) |>
  mutate(estimate_pp = estimate * to_pp[dv]) |>
  arrange(spec, dv, subsample)

write_csv(sensitivity, file.path(summ_dir, "08_sensitivity.csv"))

# Bounds and ECM summaries ------------------------------------------------

bounds_summary <- bounds_df |>
  summarize(
    n_countries = n(),
    n_error = sum(is.na(f_statistic)),
    n_strong = sum(str_detect(conclusion, "Strong"), na.rm = TRUE),
    n_evidence = sum(str_detect(conclusion, "Evidence"), na.rm = TRUE),
    n_weak = sum(str_starts(conclusion, "Weak"), na.rm = TRUE),
    n_none = sum(str_starts(conclusion, "No evidence"), na.rm = TRUE),
    pct_coint_05 = 100 * sum(str_starts(conclusion, "Cointegration:")) /
      sum(!is.na(f_statistic)),
    median_f = median(f_statistic, na.rm = TRUE),
    .by = c(spec, dv)
  )

ecm_summary <- ecm_df |>
  filter(!is.na(ect_coefficient)) |>
  summarize(
    n_countries = n(),
    median_ect = median(ect_coefficient),
    pct_stable = 100 * mean(ect_coefficient > -1 & ect_coefficient < 0),
    median_half_life = median(
      log(0.5) / log(1 + ect_coefficient[ect_coefficient > -1 &
                                           ect_coefficient < 0])
    ),
    .by = c(spec, dv)
  )

# Figures -----------------------------------------------------------------

monet <- paletteer_d("MetBrewer::Monet")

debt_lr |>
  filter(spec == "Composition") |>
  mutate(
    response = factor(response_labels[dv], levels = response_labels),
    country = reorder_within(country, longrun_multiplier, response)
  ) |>
  ggplot(aes(x = longrun_multiplier, y = country, colour = income_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#111111") +
  geom_linerange(aes(xmin = ci_lo, xmax = ci_hi), linewidth = 0.35) +
  geom_point(size = 1.4) +
  scale_y_reordered() +
  scale_colour_manual(values = monet[c(1, 3, 6, 8)]) +
  facet_wrap(~response, scales = "free", ncol = 3) +
  custom_theme() +
  theme(axis.text.y = element_text(size = 5), legend.position = "bottom") +
  labs(x = "Long-run multiplier on public debt", y = NULL, colour = NULL)

ggsave(file.path(summ_dir, "figure_longrun_debt.png"), plot = last_plot(),
       width = 12, height = 11, dpi = 300)

meta_longrun |>
  filter(variable == "public_debt") |>
  mutate(
    response = factor(response_labels[dv], levels = response_labels),
    spec = fct_rev(factor(spec, levels = names(specs)))
  ) |>
  ggplot(aes(x = estimate_pp, y = spec, colour = response)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#111111") +
  geom_linerange(aes(xmin = ci_lo * to_pp[dv], xmax = ci_hi * to_pp[dv]),
                 linewidth = 0.5) +
  geom_point(size = 2.4) +
  geom_text(aes(label = str_c("k = ", k)), nudge_y = 0.3, size = 2.2,
            colour = "#111111") +
  scale_colour_manual(values = monet[c(2, 5, 8)]) +
  facet_wrap(~response, scales = "free_x") +
  custom_theme() +
  guides(colour = "none") +
  labs(x = "Pooled long-run multiplier", y = NULL)

ggsave(file.path(summ_dir, "figure_longrun_by_spec.png"), plot = last_plot(),
       width = 9, height = 3.4, dpi = 300)

by_group |>
  filter(spec == "Composition") |>
  mutate(
    income_group = factor(income_group, levels = income_levels),
    response = factor(response_labels[dv], levels = response_labels)
  ) |>
  ggplot(aes(x = estimate, y = income_group, colour = income_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#111111") +
  geom_linerange(aes(xmin = ci.lb, xmax = ci.ub), linewidth = 0.3) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = monet[c(1, 3, 6, 8)]) +
  facet_wrap(~response, scales = "free_x") +
  custom_theme() +
  guides(colour = "none") +
  labs(x = "Pooled long-run multiplier", y = NULL)

ggsave(file.path(summ_dir, "figure_longrun_debt_by_group.png"), plot = last_plot(),
       width = 9, height = 3.4, dpi = 300)

ecm_df |>
  filter(!is.na(ect_coefficient), between(ect_coefficient, -2, 1)) |>
  mutate(
    response = factor(response_labels[dv], levels = response_labels),
    spec = factor(spec, levels = names(specs))
  ) |>
  ggplot(aes(x = ect_coefficient, fill = response)) +
  geom_histogram(bins = 25, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = monet[c(2, 5, 8)]) +
  facet_grid(spec ~ response) +
  custom_theme() +
  guides(fill = "none") +
  labs(
    x = "Speed of adjustment", y = "Countries",
    caption = "Values between -1 and 0 indicate stable adjustment."
  )

ggsave(file.path(summ_dir, "figure_ecm_distribution.png"), plot = last_plot(),
       width = 9, height = 7, dpi = 300)

bounds_df |>
  filter(!is.na(f_statistic)) |>
  mutate(
    response = factor(response_labels[dv], levels = response_labels),
    spec = factor(spec, levels = names(specs)),
    cointegrated = str_starts(conclusion, "Cointegration:")
  ) |>
  ggplot(aes(x = f_statistic, fill = cointegrated)) +
  geom_histogram(bins = 25, colour = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c("FALSE" = monet[2], "TRUE" = monet[8]),
    labels = c("FALSE" = "Not cointegrated", "TRUE" = "Cointegrated")
  ) +
  facet_grid(spec ~ response) +
  custom_theme() +
  labs(x = "Bounds test F-statistic", y = "Countries", fill = NULL)

ggsave(file.path(summ_dir, "figure_bounds_tests.png"), plot = last_plot(),
       width = 9, height = 7, dpi = 300)