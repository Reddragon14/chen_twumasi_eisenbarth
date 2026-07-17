PANEL_T     <- 30
MISS_THRESH <- 0.20
MIN_OBS     <- ceiling(PANEL_T * (1 - MISS_THRESH))

gate_vars <- c("p90p100", "public_debt", "growth")

window_search <- tibble(start = min(combined$year):(max(combined$year) - PANEL_T + 1)) |>
  mutate(
    n_countries = map_int(start, \(s) {
      combined |>
        filter(year >= s, year < s + PANEL_T) |>
        summarize(across(all_of(gate_vars), ~ sum(!is.na(.x))), .by = iso) |>
        filter(if_all(all_of(gate_vars), ~ .x >= MIN_OBS)) |>
        nrow()
    }),
    end = start + PANEL_T - 1
  ) |>
  arrange(desc(n_countries), start)

print(window_search, n = 10)

WINDOW_START <- 1989
WINDOW_END   <- 2019

kept <- combined |>
  filter(year >= WINDOW_START, year <= WINDOW_END) |>
  summarize(across(all_of(gate_vars), ~ sum(!is.na(.x))), .by = iso) |>
  filter(if_all(all_of(gate_vars), ~ .x >= MIN_OBS)) |>
  pull(iso)

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
  filter(year >= WINDOW_START, year <= WINDOW_END, iso %in% kept) |>
  arrange(iso, year) |>
  mutate(
    across(
      all_of(intersect(impute_vars, names(combined))),
      ~ imputeTS::na_locf(.x, na_remaining = "rev")
    ),
    .by = iso
  )

cat("Window:", WINDOW_START, "-", WINDOW_END, "\n")
cat("Countries:", length(kept), "\n")
cat("Rows:", nrow(combined_filtered), "of expected", length(kept) * PANEL_T, "\n")
