# =============================================================================
# Extended ARDL Analysis: Bounds Tests, Long-Run Multipliers, and ECM
# =============================================================================

library(tidyverse)
library(ARDL)
library(broom)
library(progressr)
library(metafor)
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

ardl_valid <- read_csv("./data/ardl_integration_validity.csv")
ur_classified <- read_csv("./data/unit_root_tests.csv")

valid_countries <- ardl_valid |>
  filter(ardl_valid) |>
  distinct(country) |>
  pull(country)

excluded_countries <- ur_classified |>
  filter(
    variable %in% c("p90p100", "p99p100", "public_debt"),
    integration == "I(2)+"
  ) |>
  distinct(iso, country, variable) |>
  arrange(country, variable)


df <- read_csv("updated_dataset.csv") |>
  mutate(year = as.integer(year))

df <- df |>
  mutate(
    gdp_x_debt = growth * public_debt
  ) |>
  arrange(country, year)

work <- df |>
  filter(country %in% valid_countries) |>
  select(iso, 
         country,
         income_group, 
         year, 
         p90p100,
         public_debt,
         growth, 
         life_exp, 
         gdp_x_debt)

max_lag <- 3

max_order <- max_lag
criterion <- "AIC"

n_regressors <- 4
max_params <- 1 + max_lag + n_regressors * (1 + max_lag)
min_rows <- 5 + max_params

countries <- sort(unique(work$country))

extract_longrun_multipliers <- function(ardl_model, dv = "p90p100",
                                        regressors = c("public_debt", "growth", "life_exp", "gdp_x_debt")) {
  cf <- coef(ardl_model)
  cf_names <- names(cf)
  V <- vcov(ardl_model)
  dof <- df.residual(ardl_model)

  if (is.null(cf_names)) {
    stop("coef(ardl_model) has no names; cannot match lag terms.")
  }
  if (!identical(cf_names, rownames(V))) {
    stop("Coefficient names and vcov() row names are not aligned; cannot apply delta method safely.")
  }

  c_vec <- as.numeric(grepl(paste0("^L\\(", dv, ",\\s*\\d+\\)$"), cf_names, perl = TRUE))
  sum_phi <- sum(c_vec * cf)
  D <- 1 - sum_phi

  if (abs(D) < 1e-8) {
    warning("Denominator near zero; long-run multipliers undefined (unit root).")
    return(NULL)
  }

  map_dfr(regressors, function(xvar) {
    a_vec <- as.numeric(cf_names == xvar) +
      as.numeric(grepl(paste0("^L\\(", xvar, ",\\s*\\d+\\)$"), cf_names, perl = TRUE))

    N <- sum(a_vec * cf)
    lr <- N / D

    grad <- (a_vec * D + N * c_vec) / D^2
    var_lr <- as.numeric(t(grad) %*% V %*% grad)
    se_lr <- if (is.finite(var_lr) && var_lr >= 0) sqrt(var_lr) else NA_real_

    t_stat <- lr / se_lr
    p_val <- if (!is.na(t_stat)) 2 * pt(-abs(t_stat), df = dof) else NA_real_

    tibble(
      variable           = xvar,
      n_y_lag_terms      = sum(c_vec),
      n_x_lag_terms      = sum(a_vec) - as.numeric(xvar %in% cf_names),
      sum_beta           = N,
      longrun_multiplier = lr,
      sum_phi            = sum_phi,
      se                 = se_lr,
      t_stat             = t_stat,
      p_value            = p_val,
      ci_lo              = lr - qt(0.975, dof) * se_lr,
      ci_hi              = lr + qt(0.975, dof) * se_lr
    )
  })
}

extract_ecm <- function(ardl_model) {
  tryCatch(
    {
      ecm_model <- recm(ardl_model, case = 3) # Case 3: unrestricted intercept, no trend
      sm <- summary(ecm_model)

      list(
        ecm_model     = ecm_model,
        coefficients  = coef(ecm_model),
        summary       = sm,
        r_squared     = sm$r.squared,
        adj_r_squared = sm$adj.r.squared
      )
    },
    error = function(e) {
      list(error = conditionMessage(e))
    }
  )
}


#' Perform Pesaran bounds test for cointegration using ARDL package
perform_bounds_test <- function(ardl_model) {
  tryCatch(
    {
      bt_f <- bounds_f_test(ardl_model, case = "uc")

      f_stat <- bt_f$statistic[["F"]]
      f_pvalue <- bt_f$p.value[["p_value.F"]]

      conclusion <- case_when(
        f_pvalue <= 0.01 ~ "Cointegration: Strong evidence (p <= 0.01)",
        f_pvalue <= 0.05 ~ "Cointegration: Evidence (p <= 0.05)",
        f_pvalue <= 0.10 ~ "Weak evidence of cointegration (0.05 < p <= 0.10)",
        TRUE ~ "No evidence of cointegration (p > 0.10)"
      )

      return(list(
        F_statistic = f_stat,
        F_pvalue = f_pvalue,
        conclusion = conclusion,
        case_used = "uc",
        success = TRUE
      ))
    },
    error = function(e) {
      tryCatch(
        {
          bt_f <- bounds_f_test(ardl_model, case = "rc") # restricted constant

          f_stat <- bt_f$statistic[["F"]]
          f_pvalue <- bt_f$p.value[["p_value.F"]]

          conclusion <- case_when(
            f_pvalue <= 0.01 ~ "Cointegration: Strong evidence (p <= 0.01)",
            f_pvalue <= 0.05 ~ "Cointegration: Evidence (p <= 0.05)",
            f_pvalue <= 0.10 ~ "Weak evidence of cointegration (0.05 < p <= 0.10)",
            TRUE ~ "No evidence of cointegration (p > 0.10)"
          )

          return(list(
            F_statistic = f_stat,
            F_pvalue = f_pvalue,
            conclusion = conclusion,
            case_used = "rc",
            success = TRUE
          ))
        },
        error = function(e2) {
          return(list(
            F_statistic = NA_real_,
            F_pvalue = NA_real_,
            conclusion = paste("Error:", conditionMessage(e2)),
            case_used = NA_character_,
            success = FALSE
          ))
        }
      )
    }
  )
}


#' Interpret bounds test results based on p-values
#' NOTE: this function is not currently called anywhere in the script below;
#' left in place in case a per-country narrative summary is wanted later.
interpret_bounds <- function(f_stat, f_pvalue, t_stat, t_pvalue) {
  results <- list()

  if (!is.na(f_pvalue)) {
    if (f_pvalue <= 0.01) {
      results$F_test <- "Cointegration: Strong evidence (p \u2264 0.01)"
    } else if (f_pvalue <= 0.05) {
      results$F_test <- "Cointegration: Evidence (p \u2264 0.05)"
    } else if (f_pvalue <= 0.10) {
      results$F_test <- "Weak evidence of cointegration (0.05 < p \u2264 0.10)"
    } else {
      results$F_test <- "No evidence of cointegration (p > 0.10)"
    }
  } else {
    results$F_test <- "F-test p-value not available"
  }

  if (!is.na(t_pvalue)) {
    if (t_pvalue <= 0.01) {
      results$t_test <- "Cointegration: Strong evidence (p \u2264 0.01)"
    } else if (t_pvalue <= 0.05) {
      results$t_test <- "Cointegration: Evidence (p \u2264 0.05)"
    } else if (t_pvalue <= 0.10) {
      results$t_test <- "Weak evidence of cointegration (0.05 < p \u2264 0.10)"
    } else {
      results$t_test <- "No evidence of cointegration (p > 0.10)"
    }
  } else {
    results$t_test <- "t-test p-value not available"
  }

  if (grepl("Cointegration|evidence", results$F_test) &&
    grepl("Cointegration|evidence", results$t_test)) {
    if (grepl("Strong", results$F_test) && grepl("Strong", results$t_test)) {
      results$combined <- "STRONG evidence of cointegration from both tests"
    } else {
      results$combined <- "Evidence of cointegration from both tests"
    }
  } else if (grepl("No evidence", results$F_test) &&
    grepl("No evidence", results$t_test)) {
    results$combined <- "NO evidence of cointegration from either test"
  } else if (grepl("Weak evidence", results$F_test) ||
    grepl("Weak evidence", results$t_test)) {
    results$combined <- "Mixed or weak evidence - further investigation needed"
  } else {
    results$combined <- "Mixed results - interpret with caution"
  }

  return(results)
}

# Storage lists
meta_list <- list()
coef_list <- list()
models_list <- list()
bounds_list <- list()
longrun_list <- list()
ecm_list <- list()

# -----------------------------------------------------------------------------
# One-off single-country check, left as manual diagnostic scratch code.
# Guarded so it doesn't run as part of the main script execution.
# -----------------------------------------------------------------------------
if (FALSE) {
  arg_df <- work |>
    filter(country == "Argentina") |>
    arrange(year) |>
    drop_na(p90p100, public_debt, growth, life_exp, gdp_x_debt)

  form <- p90p100 ~ public_debt + growth + life_exp + gdp_x_debt

  bm_check <- ARDL::auto_ardl(
    formula   = form,
    data      = arg_df,
    max_order = max_order,
    selection = criterion,
    na.action = na.omit
  )

  names(coef(bm_check$best_model))
  bounds_f_test(bm_check$best_model, case = "rc")
  perform_bounds_test(bm_check$best_model)
}

summ_dir <- "./data/ardl_summaries"
if (!dir.exists(summ_dir)) dir.create(summ_dir, recursive = TRUE)

with_progress({
  p <- progressor(steps = length(countries))

  for (cc in countries) {
    p(message = paste("Fitting", cc))

    sub <- work |>
      filter(country == cc) |>
      arrange(year) |>
      drop_na(p90p100, public_debt, growth, life_exp, gdp_x_debt)

    status <- "ok"
    aic <- NA_real_
    bic <- NA_real_
    best_order <- NA_character_
    nobs <- nrow(sub)

    if (nobs < min_rows) {
      status <- paste0("skipped: nobs(", nobs, ") < min_rows(", min_rows, ")")
      meta_list[[cc]] <- tibble(country = cc, status, aic, bic, nobs, best_order)
      next
    }

    form <- p90p100 ~ public_debt + growth + life_exp + gdp_x_debt

    fit <- tryCatch(
      {
        ARDL::auto_ardl(
          formula   = form,
          data      = sub,
          max_order = max_order,
          selection = criterion,
          na.action = na.omit
        )
      },
      error = function(e) e
    )

    if (inherits(fit, "error")) {
      status <- paste0("failed: ", conditionMessage(fit))
      meta_list[[cc]] <- tibble(country = cc, status, aic, bic, nobs, best_order)
      next
    }

    bm <- fit$best_model
    so <- fit$best_order
    best_order <- paste0("(", paste(so, collapse = ","), ")")

    aic <- tryCatch(AIC(bm), error = function(e) NA_real_)
    bic <- tryCatch(BIC(bm), error = function(e) NA_real_)

    meta_list[[cc]] <- tibble(
      country    = cc,
      status     = status,
      aic        = aic,
      bic        = bic,
      nobs       = nobs,
      best_order = best_order
    )

    sm <- summary(bm)
    cf <- as.data.frame(sm$coefficients)
    cf$term <- rownames(cf)
    rownames(cf) <- NULL
    coef_list[[cc]] <- cf |>
      as_tibble() |>
      transmute(
        country   = cc,
        term,
        estimate  = `Estimate`,
        std_error = `Std. Error`,
        t_value   = `t value`,
        p_value   = `Pr(>|t|)`
      )

    bt_result <- perform_bounds_test(bm)
    bounds_list[[cc]] <- tibble(
      country     = cc,
      F_statistic = if (is.null(bt_result$error)) bt_result$F_statistic else NA_real_,
      conclusion  = if (is.null(bt_result$error)) bt_result$conclusion else bt_result$error,
      case_used   = if (is.null(bt_result$error)) bt_result$case_used else NA_character_
    )

    lr_result <- extract_longrun_multipliers(bm, dv = "p90p100")
    if (!is.null(lr_result)) {
      expected_p <- so[1]
      matched_p  <- unique(lr_result$n_y_lag_terms)
      if (length(matched_p) == 1 && matched_p != expected_p) {
        warning(sprintf("%s: best_order selected p = %d but %d own-lag terms matched",
                        cc, expected_p, matched_p))
      }
      longrun_list[[cc]] <- lr_result |>
        mutate(country = cc, income_group = first(sub$income_group), .before = 1)
    }

    ecm_result <- extract_ecm(bm)
    if (is.null(ecm_result$error)) {
      ecm_cf <- coef(ecm_result$ecm_model)

      # Speed of adjustment is typically the coefficient on lagged p90p100 level
      ect_coef <- ecm_cf[grepl("L\\(p90p100,\\s*1,\\s*TRUE\\)|ect", names(ecm_cf), perl = TRUE)]
      if (length(ect_coef) == 0) ect_coef <- NA_real_

      ecm_list[[cc]] <- tibble(
        country = cc,
        ect_coefficient = as.numeric(ect_coef[1]),
        ecm_r_squared = ecm_result$r_squared
      )
    } else {
      ecm_list[[cc]] <- tibble(
        country = cc,
        ect_coefficient = NA_real_,
        ecm_r_squared = NA_real_
      )
    }

    models_list[[cc]] <- list(
      country    = cc,
      model      = bm,
      best_order = so,
      data       = sub,
      criterion  = criterion
    )

    capture.output(print(sm), file = file.path(summ_dir, paste0(cc, ".txt")))
  }
})


meta_df <- bind_rows(meta_list)
coef_df <- bind_rows(coef_list)
bounds_df <- bind_rows(bounds_list)
longrun_df <- bind_rows(longrun_list)
ecm_df <- bind_rows(ecm_list)

# Save compiled results
write_csv(meta_df, file.path(summ_dir, "00_meta_summary.csv"))
write_csv(bounds_df, file.path(summ_dir, "01_bounds_tests.csv"))
write_csv(longrun_df, file.path(summ_dir, "02_longrun_multipliers.csv"))
write_csv(ecm_df, file.path(summ_dir, "03_ecm_adjustment.csv"))


# =============================================================================
# META-ANALYSIS AND META-REGRESSION OF LONG-RUN MULTIPLIERS
# =============================================================================

longrun_clean <- longrun_df |>
  filter(is.finite(longrun_multiplier), is.finite(se), se > 0)

# Pooled random-effects estimate per variable, using the same machinery as
# the short-run terms (analyze_one_term expects columns country/yi/sei)
res_longrun <- map_dfr(c("public_debt", "growth", "life_exp", "gdp_x_debt"), function(v) {
  dat_v <- longrun_clean |>
    filter(variable == v) |>
    transmute(country, estimate = longrun_multiplier, std_error = se)
  analyze_one_term(dat_v, v)
})

write_csv(res_longrun, file.path(summ_dir, "04_meta_longrun_summary.csv"))

cat("\n=== LONG-RUN MULTIPLIERS: RANDOM-EFFECTS META-ANALYSIS ===\n")
print(res_longrun)

# ---- Meta-regression: does income group moderate the long-run debt effect? ----

debt_lr <- longrun_clean |> filter(variable == "public_debt") |>
  mutate(income_group = factor(income_group,
                               levels = c("Low income", "Lower middle income",
                                          "Upper middle income", "High income")))

# Omnibus test of whether income group explains heterogeneity in the
# long-run debt multiplier (QM / QMp, reference-coded)
rma_debt_omnibus <- rma(yi = longrun_multiplier, sei = se, mods = ~ income_group,
                        data = debt_lr, method = "REML")

cat("\n=== META-REGRESSION: LONG-RUN DEBT MULTIPLIER ~ INCOME GROUP ===\n")
print(summary(rma_debt_omnibus))

# Group-specific pooled estimates (no-intercept coding), for reporting and
# for the forest plot below
rma_debt_by_group <- rma(yi = longrun_multiplier, sei = se,
                         mods = ~ income_group - 1, data = debt_lr, method = "REML")

debt_group_summary <- coef(summary(rma_debt_by_group)) |>
  as_tibble(rownames = "income_group") |>
  mutate(income_group = str_remove(income_group, "^income_group"))

write_csv(debt_group_summary, file.path(summ_dir, "05_meta_regression_income_group.csv"))

cat("\n=== POOLED LONG-RUN DEBT MULTIPLIER BY INCOME GROUP ===\n")
print(debt_group_summary)

##### SUMMARY TABLES ####

bounds_summary <- bounds_df |>
  mutate(
    is_error     = is.na(F_statistic),
    cointegrated = grepl("^Cointegration:", conclusion)
  ) |>
  summarise(
    n_countries               = n(),
    n_error                   = sum(is_error),
    n_uc                      = sum(case_used == "uc", na.rm = TRUE),
    n_rc                      = sum(case_used == "rc", na.rm = TRUE),
    n_cointegrated            = sum(cointegrated, na.rm = TRUE),
    n_weak                    = sum(grepl("^Weak evidence", conclusion)),
    n_no_evidence             = sum(grepl("^No evidence", conclusion)),
    pct_cointegrated_of_valid = 100 * sum(cointegrated) / sum(!is_error),
    mean_F                    = mean(F_statistic, na.rm = TRUE),
    sd_F                      = sd(F_statistic, na.rm = TRUE)
  )

cat("\n=== BOUNDS TEST SUMMARY ===\n")
print(bounds_summary)

# ECM summary
ecm_summary <- ecm_df |>
  filter(!is.na(ect_coefficient)) |>
  summarise(
    n_countries = n(),
    mean_ect = mean(ect_coefficient, na.rm = TRUE),
    sd_ect = sd(ect_coefficient, na.rm = TRUE),
    pct_negative = mean(ect_coefficient < 0, na.rm = TRUE) * 100,
    pct_stable = mean(ect_coefficient > -1 & ect_coefficient < 0, na.rm = TRUE) * 100
  )

cat("\n=== ECM SPEED OF ADJUSTMENT SUMMARY ===\n")
print(ecm_summary)

# Long-run multipliers summary
cat("\n=== LONG-RUN MULTIPLIERS (POOLED) ===\n")
print(meta_longrun)

cointegrated_countries <- bounds_df |>
  filter(grepl("^Cointegration:|^Weak evidence", conclusion)) |>
  pull(country)

longrun_clean_coint <- longrun_clean |> filter(country %in% cointegrated_countries)

res_longrun_coint <- map_dfr(c("public_debt", "growth", "life_exp", "gdp_x_debt"), function(v) {
  dat_v <- longrun_clean_coint |>
    filter(variable == v) |>
    transmute(country, estimate = longrun_multiplier, std_error = se)
  analyze_one_term(dat_v, v)
})

cat("\n=== LONG-RUN MULTIPLIERS, COINTEGRATED COUNTRIES ONLY (k =", 
    length(cointegrated_countries), ") ===\n")
print(res_longrun_coint)

debt_lr_coint <- longrun_clean_coint |> filter(variable == "public_debt")

rma_debt_coint <- rma(yi = longrun_multiplier, sei = se, data = debt_lr_coint, method = "REML")

loo <- leave1out(rma_debt_coint)
loo_df <- tibble(country = debt_lr_coint$country, estimate = loo$estimate, pval = loo$pval)
loo_df |> arrange(estimate)


# =============================================================================
# VISUALIZATION
# =============================================================================

debt_lr |>
  ggplot(aes(x = longrun_multiplier, y = reorder(country, longrun_multiplier),
             color = income_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#111111") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0, linewidth = 0.4) +
  geom_point(size = 1.8) +
  scale_colour_paletteer_d("lisa::ClaudeMonet_1") +
  labs(
    title = "Long-Run Effect of Debt on Inequality",
    subtitle = "Country-specific ARDL long-run multipliers, 95% CI (delta method)",
    x = "Long-run multiplier",
    y = NULL,
    color = "Income group"
  ) +
  custom_theme() +
  theme(axis.text.y = element_text(size = 7), legend.position = "bottom")


ggsave(file.path(summ_dir, "figure_longrun_debt.png"), p_debt_lr,
       width = 8, height = 11, dpi = 300)

# ---- Meta-regression result: pooled long-run debt multiplier by income group ----

debt_group_summary |>
  mutate(income_group = factor(income_group,
                               levels = c("Low income", "Lower middle income",
                                          "Upper middle income", "High income"))) |>
  ggplot(aes(x = estimate, y = income_group, color = income_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#111111") +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), height = 0.15, linewidth = 0.6) +
  geom_point(size = 3) +
  scale_colour_paletteer_d("lisa::ClaudeMonet_1") +
  labs(
    title = "Pooled Long-Run Debt Multiplier by Income Group",
    subtitle = sprintf("Random-effects meta-regression; QM = %.2f, df = %d, p = %.3f",
                       rma_debt_omnibus$QM, rma_debt_omnibus$m - 1, rma_debt_omnibus$QMp),
    x = "Pooled long-run multiplier",
    y = NULL
  ) +
  custom_theme() +
  theme(legend.position = "none")

ggsave(file.path(summ_dir, "figure_longrun_debt_by_group.png"), p_debt_by_group,
       width = 7, height = 4, dpi = 300)

# ---- ECM coefficient distribution ----
ecm_df |>
  filter(!is.na(ect_coefficient)) |>
  ggplot(aes(x = ect_coefficient)) +
  geom_histogram(aes(fill = after_stat(count) > -1), bins = 20, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#111111") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "#111111") +
  scale_fill_paletteer_d("lisa::ClaudeMonet_1", guide = "none") +
  labs(
    title = "Distribution of Error Correction Coefficients",
    subtitle = "Values between -1 and 0 indicate stable adjustment",
    x = "Speed of adjustment (ECT coefficient)",
    y = "Count",
    caption = "Dotted line at -1 (stability boundary). Dashed line at 0."
  ) +
  custom_theme()

ggsave(file.path(summ_dir, "figure_ecm_distribution.png"), p_ecm,
       width = 8, height = 5, dpi = 300)

# ---- Bounds test F-statistics ----

bounds_df |>
  filter(!is.na(F_statistic)) |>
  mutate(cointegrated = grepl("^Cointegration:", conclusion)) |>
  ggplot(aes(x = F_statistic, y = reorder(country, F_statistic), color = cointegrated)) +
  geom_point(size = 2) +
  scale_colour_paletteer_d("lisa::ClaudeMonet_1",
                           labels = c("FALSE" = "Not cointegrated", "TRUE" = "Cointegrated"),
                           name = NULL) +
  labs(
    title = "Pesaran Bounds Test F-Statistics",
    x = "F-statistic",
    y = NULL
  ) +
  custom_theme() +
  theme(axis.text.y = element_text(size = 7), legend.position = "bottom")


ggsave(file.path(summ_dir, "figure_bounds_tests.png"), p_bounds,
       width = 8, height = 10, dpi = 300)
