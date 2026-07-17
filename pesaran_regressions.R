library(tidyverse)
library(broom)

# ============================================================
# VARIABLE CONSTRUCTION
# ============================================================

panel <- final |>
  arrange(iso, year) |>
  mutate(
    # Log debt-to-GDP (Chudik et al. use log of debt ratio)
    log_debt = log(public_debt),
    
    # First differences
    dy = p90p100 - dplyr::lag(p90p100),           # ??y_it (change in inequality)
    dd = log_debt - dplyr::lag(log_debt),         # ??d_it (change in log debt)
    dd2 = dd - dplyr::lag(dd),                    # ??²d_it (second difference of log debt)
    
    # Lags
    dy_l1 = dplyr::lag(dy),
    dy_l2 = dplyr::lag(dy),
    dd_l1 = dplyr::lag(dd),
    dd_l2 = dplyr::lag(dd),
    dd2_l1 = dplyr::lag(dd2),
    dd2_l2 = dplyr::lag(dd2),
    
    # Inflation (for extended specification)
    d_inf = weo_cpi_inflation - dplyr::lag(weo_cpi_inflation),
    d_inf_l1 = dplyr::lag(d_inf),
    d_inf_l2 = dplyr::lag(d_inf),
    .by = iso
  )

# ============================================================
# CROSS-SECTION AVERAGES (for CS-ARDL / CS-DL augmentation)
# ============================================================

cs_means <- panel |>
  summarize(
    dy_bar = mean(dy, na.rm = TRUE),
    dd_bar = mean(dd, na.rm = TRUE),
    dd2_bar = mean(dd2, na.rm = TRUE),
    d_inf_bar = mean(d_inf, na.rm = TRUE),
    .by = year
  ) |>
  arrange(year) |>
  mutate(
    dy_bar_l1 = lag(dy_bar),
    dy_bar_l2 = lag(dy_bar),
    dd_bar_l1 = lag(dd_bar),
    dd_bar_l2 = lag(dd_bar),
  )

panel <- panel |>
  left_join(cs_means, by = "year")

panel_pd <- pdata.frame(panel, index = c("country", "year"))

# ============================================================
# MODEL SPECIFICATIONS
# ============================================================
# Following Chudik et al. (2017):
#   DL:    ??y_it = c_i + ??_i ??d_it + ?? ??_i??? ??²d_{i,t-???} + v_it
#   CS-DL: ... + ??_{i,y} ????_t + ?? ??_{i???,d} ??d??_{t-???} + u_it
#
# ??_i is the long-run effect of debt accumulation on inequality
# Mean Group estimator: ??^_MG = (1/N) ?? ??^_i

specs <- list(
  # --- Distributed Lag (DL) ---
  dl_p1 = dy ~ dd + dd2,
  dl_p2 = dy ~ dd + dd2 + dd2_l1,
  dl_p3 = dy ~ dd + dd2 + dd2_l1 + dd2_l2,
  
  # --- CS-DL (cross-section augmented) ---
  csdl_p1 = dy ~ dd + dd2 +
    dy_bar + dd_bar + dd_bar_l1,
  csdl_p2 = dy ~ dd + dd2 + dd2_l1 +
    dy_bar + dd_bar + dd_bar_l1 + dd_bar_l2,
  csdl_p3 = dy ~ dd + dd2 + dd2_l1 + dd2_l2 +
    dy_bar + dd_bar + dd_bar_l1 + dd_bar_l2,
  
  # --- ARDL ---
  ardl_11 = dy ~ dy_l1 + dd + dd_l1,
  ardl_22 = dy ~ dy_l1 + dy_l2 + dd + dd_l1 + dd_l2,
  
  # --- CS-ARDL (cross-section augmented) ---
  csardl_11 = dy ~ dy_l1 + dd + dd_l1 +
    dy_bar + dy_bar_l1 + dd_bar + dd_bar_l1,
  csardl_22 = dy ~ dy_l1 + dy_l2 + dd + dd_l1 + dd_l2 +
    dy_bar + dy_bar_l1 + dy_bar_l2 + dd_bar + dd_bar_l1 + dd_bar_l2,
  
  # --- CS-DL with inflation ---
  csdl_inf_p1 = dy ~ dd + dd2 + d_inf +
    dy_bar + dd_bar + dd_bar_l1 + d_inf_bar,
  csdl_inf_p2 = dy ~ dd + dd2 + dd2_l1 + d_inf + d_inf_l1 +
    dy_bar + dd_bar + dd_bar_l1 + dd_bar_l2 + d_inf_bar
)

# ============================================================
# MEAN GROUP ESTIMATION
# ============================================================

estimate_mg <- function(data, formula, coef_name = "dd") {
  # Country-by-country OLS, then average coefficients
  data |>
    nest(.by = c(iso, country, income_group)) |>
    mutate(
      fit = map(data, \(d) {
        tryCatch(lm(formula, data = d), error = \(e) NULL)
      }),
      tidied = map(fit, \(f) {
        if (is.null(f)) return(tibble())
        tidy(f, conf.int = TRUE)
      }),
      n_obs = map_int(data, \(d) {
        tryCatch(nobs(lm(formula, data = d)), error = \(e) NA_integer_)
      })
    ) |>
    unnest(tidied) |>
    filter(term == coef_name)
}

mg_summary <- function(estimates) {
  # Mean Group: average country-specific coefficients
  # SE via Pesaran & Smith (1995): se = sd(??^_i) / sqrt(N)
  estimates |>
    summarize(
      n_countries = n(),
      phi_mg = mean(estimate),
      se_mg = sd(estimate) / sqrt(n()),
      t_stat = phi_mg / se_mg,
      p_value = 2 * pt(-abs(t_stat), df = n() - 1),
      median_phi = median(estimate),
      .by = any_of("income_group")
    )
}

# ============================================================
# RUN ALL SPECIFICATIONS
# ============================================================

results <- imap(specs, \(formula, spec_name) {
  cat("Estimating:", spec_name, "\n")
  
  est <- estimate_mg(panel, formula)
  
  bind_rows(
    mg_summary(est) |> mutate(group = "All countries"),
    mg_summary(est) |> mutate(group = income_group)
  ) |>
    mutate(spec = spec_name, .before = 1)
})

results_table <- bind_rows(results)

results_table |>
  select(spec, group, n_countries, phi_mg, se_mg, t_stat, p_value) |>
  mutate(
    phi_mg = round(phi_mg, 5),
    se_mg = round(se_mg, 5),
    t_stat = round(t_stat, 3),
    sig = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      .default = ""
    )
  ) |>
  print(n = 100)

cd_specs <- tibble(
  spec    = c("dl_p1", "csdl_p1", "ardl_11", "csardl_11"),
  formula = list(specs$dl_p1, specs$csdl_p1, specs$ardl_11, specs$csardl_11)
)

plm_results <- cd_specs |>
  mutate(
    fit = map(formula, \(f) {
      tryCatch(
        plm(f, data = panel_pd, model = "within"),
        error = \(e) { message("plm failed: ", e$message); NULL }
      )
    })
  )

# Print model summaries
walk2(plm_results$spec, plm_results$fit, \(s, f) {
  cat("\n========================================\n")
  cat("Spec:", s, "\n")
  cat("========================================\n")
  if (is.null(f)) { cat("Model failed to estimate\n"); return() }
  print(summary(f))
})

# CD test
cd_results <- plm_results |>
  mutate(
    cd = map(fit, \(f) {
      if (is.null(f)) return(tibble(cd_stat = NA_real_, p_value = NA_real_))
      test <- pcdtest(f, test = "cd")
      tibble(
        cd_stat = round(as.numeric(test$statistic), 3),
        p_value = round(test$p.value, 4)
      )
    })
  ) |>
  unnest(cd) |>
  select(spec, cd_stat, p_value)

cat("\n=== PESARAN (2004) CD TEST (H0: cross-section independence) ===\n")
print(cd_results)

