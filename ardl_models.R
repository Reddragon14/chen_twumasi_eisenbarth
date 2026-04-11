library(tidyverse)
library(ARDL)
library(broom)
library(plm)
library(progressr)

final$inflation


panel <- final |>
  arrange(iso, year) |>
  mutate(
    log_debt = log(public_debt),
    log_gdp_pc = log(gdp_pc),

    # First differences
    dy = p90p100 - dplyr::lag(p90p100),
    dd = log_debt - dplyr::lag(log_debt),
    d_inf = composite_inflation - dplyr::lag(composite_inflation),
    d_gov = govt_expenditure - dplyr::lag(govt_expenditure),
    d_mil = military_exp - dplyr::lag(military_exp),
    d_trade = trade_openness - dplyr::lag(trade_openness),
    d_log_gdp = log_gdp_pc - dplyr::lag(log_gdp_pc),
    .by = iso
  )

panel <- panel |> 
  select(iso,
         country,
         income_group,
         year,
         log_debt,
         log_gdp_pc,
         dy,
         dd,
         d_inf,
         d_gov,
         d_mil,
         d_trade,
         d_log_gdp)

##### CROSS-SECTION AVERAGES ####

cs_means <- panel |>
  summarize(
    dy_bar = mean(dy, na.rm = TRUE),
    dd_bar = mean(dd, na.rm = TRUE),
    d_inf_bar = mean(d_inf, na.rm = TRUE),
    d_gov_bar = mean(d_gov, na.rm = TRUE),
    d_log_gdp_bar = mean(d_log_gdp, na.rm = TRUE),
    .by = year
  )

panel <- panel |>
  left_join(cs_means, by = "year")

#### SPECIFICATIONS ####

# Baseline
spec_base <- dy ~ dd

# With controls
spec_ctrl <- dy ~ dd + d_inf + d_gov + d_log_gdp

# Extended controls
spec_ext <- dy ~ dd + d_inf + d_gov + d_mil + d_trade + d_log_gdp

# CS-augmented baseline
spec_cs_base <- dy ~ dd + dy_bar + dd_bar

# CS-augmented with controls
spec_cs_ctrl <- dy ~ dd + d_inf + d_gov + d_log_gdp +
  dy_bar + dd_bar + d_inf_bar + d_gov_bar + d_log_gdp_bar

all_specs <- list(
  base = spec_base,
  ctrl = spec_ctrl,
  ext = spec_ext,
  cs_base = spec_cs_base,
  cs_ctrl = spec_cs_ctrl
)


##### COUNTRY-BY-COUNTRY ARDL ESTIMATION #####

MAX_ORDER <- 2
CRITERION <- "AIC"

estimate_country_ardl <- function(data, formula, max_order, criterion) {
  # Count regressors to set max_order vector
  n_vars <- length(all.vars(formula))
  max_ord_vec <- rep(max_order, n_vars - 1)

  tryCatch(
    {
      fit <- auto_ardl(
        formula = formula,
        data = data,
        max_order = MAX_ORDER,
        selection = criterion
      )

      bm <- fit$best_model
      sm <- summary(bm)

      # Coefficients
      coefs <- tidy(bm) |>
        mutate(
          conf.low = estimate - 1.96 * std.error,
          conf.high = estimate + 1.96 * std.error
        )

      # Long-run multiplier for dd
      cf <- coef(bm)
      cf_names <- names(cf)

      y_lag_coefs <- cf[grepl("^L\\(dy,", cf_names)]
      sum_phi <- sum(y_lag_coefs, na.rm = TRUE)
      denom <- 1 - sum_phi

      dd_contemp <- cf[cf_names == "dd"]
      if (length(dd_contemp) == 0) dd_contemp <- 0
      dd_lags <- cf[grepl("^L\\(dd,", cf_names)]
      sum_beta_dd <- sum(dd_contemp, dd_lags, na.rm = TRUE)
      lr_dd <- if (abs(denom) > 1e-8) sum_beta_dd / denom else NA_real_

      # All long-run multipliers
      regressors <- all.vars(formula)[-1] # drop dependent variable
      lr_all <- map_dfr(regressors, \(xvar) {
        x_contemp <- cf[cf_names == xvar]
        if (length(x_contemp) == 0) x_contemp <- 0
        x_lags <- cf[grepl(paste0("^L\\(", xvar, ","), cf_names)]
        sum_beta <- sum(x_contemp, x_lags, na.rm = TRUE)
        lr <- if (abs(denom) > 1e-8) sum_beta / denom else NA_real_

        tibble(variable = xvar, sum_beta = sum_beta, lr_multiplier = lr)
      })

      # Bounds test
      bounds <- tryCatch(
        {
          bt <- bounds_f_test(bm, case = "uc")
          tibble(
            f_stat = bt$statistic[["F"]],
            f_pvalue = bt$p.value[["p_value.F"]]
          )
        },
        error = \(e) tibble(f_stat = NA_real_, f_pvalue = NA_real_)
      )

      # ECM
      ecm <- tryCatch(
        {
          ecm_model <- recm(bm, case = 3)
          ecm_cf <- coef(ecm_model)
          ect <- ecm_cf[grepl("ect", names(ecm_cf), ignore.case = TRUE)]
          if (length(ect) == 0) {
            ect <- ecm_cf[grepl("^L\\(dy,\\s*1,\\s*TRUE\\)", names(ecm_cf))]
          }
          tibble(
            ect_coef = if (length(ect) > 0) as.numeric(ect[1]) else NA_real_,
            ecm_r2 = summary(ecm_model)$r.squared
          )
        },
        error = \(e) tibble(ect_coef = NA_real_, ecm_r2 = NA_real_)
      )

      list(
        status = "ok",
        best_order = paste0("(", paste(fit$best_order, collapse = ","), ")"),
        n_obs = nobs(bm),
        r2 = sm$r.squared,
        adj_r2 = sm$adj.r.squared,
        aic = AIC(bm),
        bic = BIC(bm),
        sigma = sm$sigma,
        coefs = coefs,
        lr_multipliers = lr_all,
        lr_dd = lr_dd,
        sum_phi = sum_phi,
        bounds = bounds,
        ecm = ecm
      )
    },
    error = \(e) {
      list(status = paste0("failed: ", conditionMessage(e)))
    }
  )
}

#### RUN ALL SPECS × ALL COUNTRIES ####

countries <- sort(unique(panel$iso))
min_rows <- 30

results_all <- list()

with_progress({
  p <- progressor(steps = length(all_specs) * length(countries))

  for (spec_name in names(all_specs)) {
    formula <- all_specs[[spec_name]]
    dep_vars <- all.vars(formula)

    for (cc in countries) {
      p(message = paste(spec_name, cc))

      sub <- panel |>
        filter(iso == cc) |>
        arrange(year) |>
        drop_na(all_of(dep_vars))

      country_name <- sub$country[1]
      income_grp <- sub$income_group[1]

      if (nrow(sub) < min_rows) {
        results_all[[paste(spec_name, cc, sep = "|")]] <- tibble(
          spec = spec_name, iso = cc, country = country_name,
          income_group = income_grp, status = "insufficient_obs",
          n_obs = nrow(sub)
        )
        next
      }

      res <- estimate_country_ardl(sub, formula, MAX_ORDER, CRITERION)

      if (res$status != "ok") {
        results_all[[paste(spec_name, cc, sep = "|")]] <- tibble(
          spec = spec_name, iso = cc, country = country_name,
          income_group = income_grp, status = res$status,
          n_obs = nrow(sub)
        )
        next
      }

      results_all[[paste(spec_name, cc, sep = "|")]] <- tibble(
        spec = spec_name,
        iso = cc,
        country = country_name,
        income_group = income_grp,
        status = res$status,
        best_order = res$best_order,
        n_obs = res$n_obs,
        r2 = res$r2,
        adj_r2 = res$adj_r2,
        aic = res$aic,
        bic = res$bic,
        sigma = res$sigma,
        lr_dd = res$lr_dd,
        sum_phi = res$sum_phi,
        f_stat = res$bounds$f_stat,
        f_pvalue = res$bounds$f_pvalue,
        ect_coef = res$ecm$ect_coef,
        ecm_r2 = res$ecm$ecm_r2,
        coefs = list(res$coefs),
        lr_multipliers = list(res$lr_multipliers)
      )
    }
  }
})

meta <- bind_rows(results_all)

#' Model summary
model_summary <- meta |>
  select(
    spec, 
    iso, 
    country,
    income_group,
    status, 
    best_order,
    n_obs,
    r2,
    adj_r2,
    aic, 
    bic,
    sigma,
    lr_dd,
    sum_phi,
    f_stat, 
    f_pvalue,
    ect_coef, 
    ecm_r2
  )

#' Coefficients (unnested)
coef_df <- meta |>
  filter(status == "ok") |>
  select(spec, iso, country, income_group, coefs) |>
  unnest(coefs)

#' Long-run multipliers (unnested)
lr_df <- meta |>
  filter(status == "ok") |>
  select(spec, iso, country, income_group, lr_multipliers) |>
  unnest(lr_multipliers)

#### MEAN GROUP ESTIMATES (Pesaran & Smith 1995) ####

mg_estimates <- model_summary |>
  filter(status == "ok", is.finite(lr_dd)) |>
  summarize(
    n = n(),
    phi_mg = mean(lr_dd),
    se_mg = sd(lr_dd) / sqrt(n()),
    t_stat = phi_mg / se_mg,
    p_value = 2 * pt(-abs(t_stat), df = n() - 1),
    median_phi = median(lr_dd),
    q25 = quantile(lr_dd, 0.25),
    q75 = quantile(lr_dd, 0.75),
    .by = c(spec, income_group)
  ) |>
  mutate(sig = case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    .default = ""
  ))

# Pooled (all countries)
mg_pooled <- model_summary |>
  filter(status == "ok", is.finite(lr_dd)) |>
  summarize(
    n = n(),
    phi_mg = mean(lr_dd),
    se_mg = sd(lr_dd) / sqrt(n()),
    t_stat = phi_mg / se_mg,
    p_value = 2 * pt(-abs(t_stat), df = n() - 1),
    median_phi = median(lr_dd),
    .by = spec
  ) |>
  mutate(
    income_group = "All",
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      .default = ""
    )
  )

mg_table <- bind_rows(mg_pooled, mg_estimates) |>
  arrange(spec, income_group)

cat("\n=== MEAN GROUP: LONG-RUN EFFECT OF DEBT ON INEQUALITY ===\n")
mg_table |>
  select(spec, income_group, n, phi_mg, se_mg, t_stat, sig, median_phi) |>
  mutate(across(c(phi_mg, se_mg, median_phi), ~ round(.x, 5)),
    t_stat = round(t_stat, 3)
  ) |>
  print(n = 100)

#### DIAGNOSTIC SUMMARIES #####

# Bounds tests
cat("\n=== BOUNDS TEST SUMMARY ===\n")
model_summary |>
  filter(status == "ok") |>
  mutate(cointegrated = f_pvalue <= 0.05) |>
  summarize(
    n = n(),
    n_coint = sum(cointegrated, na.rm = TRUE),
    pct_coint = round(100 * mean(cointegrated, na.rm = TRUE), 1),
    mean_f = round(mean(f_stat, na.rm = TRUE), 2),
    .by = spec
  ) |>
  print()

# ECM speed of adjustment
cat("\n=== ECM ADJUSTMENT SPEED ===\n")
model_summary |>
  filter(status == "ok", !is.na(ect_coef)) |>
  summarize(
    n = n(),
    mean_ect = round(mean(ect_coef), 4),
    pct_negative = round(100 * mean(ect_coef < 0), 1),
    pct_stable = round(100 * mean(ect_coef > -1 & ect_coef < 0), 1),
    median_halflife = round(log(2) / abs(median(ect_coef[ect_coef < 0])), 1),
    .by = spec
  ) |>
  print()

# Estimation success rates
cat("\n=== ESTIMATION STATUS ===\n")
meta |>
  count(spec, status) |>
  pivot_wider(names_from = status, values_from = n, values_fill = 0) |>
  print()

#### CD TEST (on pooled within models for comparison) ####

panel_pd <- pdata.frame(panel, index = c("iso", "year"))

cd_specs <- list(
  dl_base = dy ~ dd,
  dl_ctrl = dy ~ dd + d_inf + d_gov + d_log_gdp,
  csdl_ctrl = dy ~ dd + d_inf + d_gov + d_log_gdp +
    dy_bar + dd_bar + d_inf_bar + d_gov_bar + d_log_gdp_bar
)

cd_results <- imap_dfr(cd_specs, \(f, name) {
  fit <- tryCatch(plm(f, data = panel_pd, model = "within"), error = \(e) NULL)
  if (is.null(fit)) {
    return(tibble(spec = name, cd_stat = NA, cd_p = NA))
  }

  cd <- pcdtest(fit, test = "cd")
  tibble(
    spec = name,
    cd_stat = round(as.numeric(cd$statistic), 3),
    cd_p = round(cd$p.value, 4)
  )
})

cat("\n=== PESARAN CD TEST ===\n")
print(cd_results)


write_csv(model_summary, "./data/ardl_model_summary.csv")
write_csv(coef_df, "./data/ardl_coefficients.csv")
write_csv(lr_df, "./data/ardl_longrun_multipliers.csv")
write_csv(mg_table, "./data/ardl_mean_group.csv")
