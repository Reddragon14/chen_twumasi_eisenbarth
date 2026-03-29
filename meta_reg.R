# Coerce year to integer (best effort)
df <- dataset %>% mutate(year = as.integer(year))

# Create dependent variable and interaction
df <- df %>%
  mutate(log_gini   = log(gini_imputed),
         gdp_x_debt = gdp_imputed * debt_imp) %>%
  arrange(country, year)

# Keep only needed columns
work <- df %>%
  select(country, year, log_gini, debt_imp, gdp_imputed, hc, gdp_x_debt)

# ---------- Model settings ----------
# max_order: c(max_lag_y, max_lag_x1, ..., max_lag_xk)
# Here k = 4 (debt_imp, gdp_imputed, hc, gdp_x_debt)
max_lag <- 4
max_order <- c(max_lag, max_lag, max_lag, max_lag, max_lag)
criterion <- "AIC"   # or "BIC"

# Minimum effective rows to attempt a fit (simple heuristic)
min_rows <- 20 + sum(max_order, na.rm = TRUE)

countries <- sort(unique(work$country))

meta_list   <- list()
coef_list   <- list()
models_list <- list()

# Output dir for text summaries & models
summ_dir <- "/data/ardl_summaries"
if (!dir.exists(summ_dir))
  dir.create(summ_dir, recursive = TRUE)

# ---------- Run with progress ----------
with_progress({
  p <- progressor(steps = length(countries))

  for (cc in countries) {
    p(message = paste("Fitting", cc))

    sub <- work %>%
      filter(country == cc) %>%
      arrange(year) %>%
      distinct(year, .keep_all = TRUE) %>%
      drop_na(log_gini, debt_imp, gdp_imputed, hc, gdp_x_debt)

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

    form <- log_gini ~ debt_imp + gdp_imputed + hc + gdp_x_debt

    fit <- tryCatch({
      ARDL::auto_ardl(
        formula    = form,
        data       = sub,
        max_order  = max_order,
        selection  = criterion
      )
    }, error = function(e)
      e)

    if (inherits(fit, "error")) {
      status <- paste0("failed: ", conditionMessage(fit))
      meta_list[[cc]] <- tibble(country = cc, status, aic, bic, nobs, best_order)
      next
    }

    bm <- fit$best_model
    so <- fit$best_order
    best_order <- paste0("(", paste(so, collapse = ","), ")")

    aic <- tryCatch(
      AIC(bm),
      error = function(e)
        NA_real_
    )
    bic <- tryCatch(
      BIC(bm),
      error = function(e)
        NA_real_
    )

    meta_list[[cc]] <- tibble(
      country    = cc,
      status     = status,
      aic        = aic,
      bic        = bic,
      nobs       = nobs,
      best_order = best_order
    )

    # Coef table
    sm <- summary(bm)
    cf <- as.data.frame(sm$coefficients)
    cf$term <- rownames(cf)
    rownames(cf) <- NULL
    coef_list[[cc]] <- cf %>%
      as_tibble() %>%
      transmute(
        country = cc,
        term,
        estimate = `Estimate`,
        std_error = `Std. Error`,
        t_value = `t value`,
        p_value = `Pr(>|t|)`
      )

    # Save plain-text summary
    capture.output(print(sm), file = file.path(summ_dir, paste0(cc, ".txt")))

    # ---- store final model ----
    models_list[[cc]] <- list(
      country    = cc,
      model      = bm,
      # lm object
      best_order = so,
      # c(p, q1, q2, q3, q4)
      data       = sub,
      # data actually used
      criterion  = criterion
    )
  }
})

meta_df <- bind_rows(meta_list)
coef_df <- bind_rows(coef_list)


base_dir <- "/data/ardl_summaries"
if (!dir.exists(base_dir))
  base_dir <- getwd()

# ---- Basic checks & cleaning ----
stopifnot(all(
  c("country", "term", "estimate", "std_error") %in% names(coef_df)
))

# ---- Clean input ----
coef_df_clean <- coef_df %>%
  filter(is.finite(estimate), is.finite(std_error), std_error > 0)

# ---- Helpers ----
sanitize_term <- function(x) {
  x %>%
    str_replace_all("`", "") %>%
    str_replace_all("[^A-Za-z0-9_.-]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace("^_|_$", "")
}

# Robust CI extractor for rma.uni fits
safe_ci <- function(fit) {
  ci_df <- tryCatch(
    as.data.frame(confint(fit)),
    error = function(e)
      NULL
  )
  if (!is.null(ci_df)) {
    if (!is.null(rownames(ci_df)) && "intrcpt" %in% rownames(ci_df) &&
        all(c("ci.lb", "ci.ub") %in% colnames(ci_df))) {
      return(c(lo = as.numeric(ci_df["intrcpt", "ci.lb"]), hi = as.numeric(ci_df["intrcpt", "ci.ub"])))
    }
    if (all(c("ci.lb", "ci.ub") %in% names(ci_df))) {
      return(c(
        lo = as.numeric(ci_df$ci.lb[1]),
        hi = as.numeric(ci_df$ci.ub[1])
      ))
    }
  }
  pr <- tryCatch(
    predict(fit),
    error = function(e)
      NULL
  )
  if (!is.null(pr) && all(c("ci.lb", "ci.ub") %in% names(pr))) {
    return(c(lo = as.numeric(pr$ci.lb[1]), hi = as.numeric(pr$ci.ub[1])))
  }
  c(lo = NA_real_, hi = NA_real_)
}

# ---- Analyze a single term → one summary row ----
analyze_one_term <- function(dat_term, term_label) {
  dat <- dat_term %>% transmute(country, yi = estimate, sei = std_error)
  if (nrow(dat) < 3) {
    return(tibble(
      term    = term_label, k = nrow(dat), status = "skipped_k<3",
      beta_RE = NA_real_, se_RE = NA_real_, z_RE = NA_real_, p_RE = NA_real_,
      ci95_lo = NA_real_, ci95_hi = NA_real_,
      tau2    = NA_real_, I2 = NA_real_, Q = NA_real_, Q_df = NA_real_, Q_p = NA_real_
    ))
  }
  fit <- tryCatch(rma.uni(yi = dat$yi, sei = dat$sei, method = "REML"),
                  error = function(e) e)
  if (inherits(fit, "error")) {
    return(tibble(
      term    = term_label, k = nrow(dat),
      status  = paste0("failed: ", conditionMessage(fit)),
      beta_RE = NA_real_, se_RE = NA_real_, z_RE = NA_real_, p_RE = NA_real_,
      ci95_lo = NA_real_, ci95_hi = NA_real_,
      tau2    = NA_real_, I2 = NA_real_, Q = NA_real_, Q_df = NA_real_, Q_p = NA_real_
    ))
  }
  ci <- safe_ci(fit)
  tibble(
    term    = term_label,
    k       = fit$k,
    status  = "ok",
    beta_RE = as.numeric(fit$b),
    se_RE   = as.numeric(fit$se),
    z_RE    = as.numeric(fit$zval),
    p_RE    = as.numeric(fit$pval),
    ci95_lo = ci[["lo"]],
    ci95_hi = ci[["hi"]],
    tau2    = as.numeric(fit$tau2),
    I2      = as.numeric(fit$I2),
    Q       = as.numeric(fit$QE),
    Q_df    = as.numeric(fit$k - 1),
    Q_p     = as.numeric(fit$QEp)
  )
}

# ---- Meta-analyze a vector of terms → data frame + CSV ----
meta_analyze_terms <- function(terms, file_prefix) {
  out <- map_dfr(terms, function(tt) {
    dat_tt <- coef_df_clean %>% filter(term == tt)
    analyze_one_term(dat_tt, tt)
  }) %>% arrange(term)
  write_csv(out, file.path(base_dir, paste0(file_prefix, "_summary.csv")))
  out
}

# ------------------- RUNS -------------------

# 1) Base (contemporaneous) terms only
base_terms <- c("debt_imp", "gdp_imputed", "hc", "gdp_x_debt")
res_base <- meta_analyze_terms(base_terms, file_prefix = "meta_contemporaneous")

# 2) ALL terms in coef_df (including lags)
all_terms <- sort(unique(coef_df_clean$term))
res_all <- meta_analyze_terms(all_terms, file_prefix = "meta_all_terms")

res_all %>%
  mutate(term = factor(term)) %>%
  filter(!is.na(beta_RE)) %>%
  ggplot(aes(x = beta_RE, y = fct_reorder(term, beta_RE))) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Pooled ARDL Coefficients Across Countries",
    x = "Pooled estimate (random-effects, 95% CI)",
    y = NULL,
    caption = "Effect sizes from country-level ARDL fits; pooled via REML."
  ) +
  scale_x_continuous(labels = label_number(), guide = guide_axis(minor.ticks = TRUE))
