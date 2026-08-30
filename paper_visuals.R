library(tidyverse)
library(paletteer)
library(extrafont)
ggthemr::ggthemr(palette = "greyscale", layout = "clean", type = "outer")

custom_theme <- function() {
  theme(
    text              = element_text(family = "Arial", colour = "#111111"),
    plot.title        = element_text(size = 11, face = "plain", colour = "#111111",
                                     margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 9, colour = "#111111", lineheight = 1.4,
                                     margin = margin(b = 14)),
    plot.caption      = element_text(size = 8, colour = "#111111", hjust = 0,
                                     margin = margin(t = 10)),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.background  = element_rect(fill = "white", colour = NA),
    panel.border      = element_blank(),
    axis.line         = element_line(colour = "#111111", linewidth = 0.4),
    axis.ticks        = element_line(colour = "#111111", linewidth = 0.35),
    axis.ticks.length = unit(-5, "pt"),
    axis.title        = element_text(size = 9, colour = "#111111"),
    axis.text         = element_text(size = 8, colour = "#111111"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.position   = "bottom",
    legend.key        = element_rect(fill = NA, colour = NA),
    legend.title      = element_text(size = 8, colour = "#111111"),
    legend.text       = element_text(size = 8, colour = "#111111"),
    plot.margin       = margin(16, 20, 12, 16)
  )
}

raw <- read_csv("initial_data.csv")


measure_labels <- c(
  p90p100          = "Top 10% Income Share",
  p99p100          = "Top 1% Income Share",
  gini_mean        = "Gini Coefficient",
  public_debt      = "Public Debt",
  ggd_private_debt = "Private Debt",
  ggd_nfc_debt     = "Non-Financial Corporate Debt",
  ggd_hh_debt      = "Household Debt"
)

raw_ts <- raw |>
  mutate(gini_mean = gini_mean / 100) |>
  select(iso, year, rgdpo, all_of(names(measure_labels))) |>
  pivot_longer(all_of(names(measure_labels)), names_to = "measure", values_to = "value") |>
  filter(!is.na(value), !is.na(rgdpo)) |>
  mutate(mu = weighted.mean(value, rgdpo), .by = measure) |>
  mutate(cm = weighted.mean(value, rgdpo), .by = c(measure, iso)) |>
  summarize(
    value = weighted.mean(value - cm + mu, rgdpo),
    n_countries = n_distinct(iso),
    .by = c(measure, year)
  ) |>
  mutate(measure = measure_labels[measure]) |>
  arrange(measure, year)

events_all <- tibble(
  xmin  = c(1979, 1981),
  xmax  = c(1990, 1989),
  label = c("Thatcher", "Reagan")
)

events_hh <- tibble(
  measure = "Household Debt",
  year    = c(1958, 1978),
  label   = c("BankAmericard", "Marquette")
)

inequality_measures <- c("Top 10% Income Share", "Top 1% Income Share")
debt_measures <- c("Public Debt", "Private Debt", "Non-Financial Corporate Debt", "Household Debt")

# Inequality measures, raw data
raw_ts |>
  filter(measure %in% inequality_measures) |>
  ggplot(aes(x = year)) +
  geom_rect(data = events_all,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = as.factor(label)),
            alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = value, color = measure)) +
  scale_x_continuous(guide  = guide_axis(minor.ticks = TRUE),
                     breaks = seq(1950, 2020, by = 20)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     guide  = guide_axis(minor.ticks = TRUE)) +
  scale_colour_paletteer_d("MoMAColors::Lupi", direction = -1) +
  scale_fill_manual(values = c("Reagan" = "#E5E5E5", "Thatcher" = "#E5E5E5")) +
  custom_theme() +
  facet_wrap(~ measure, scales = "free") +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL)

ggsave(plot = last_plot(), "./figures/inequality_measures.png",
       dpi = 600, width = 7, height = 3, limitsize = FALSE)

# Debt measures, raw data
raw_ts |>
  filter(measure %in% debt_measures) |>
  ggplot(aes(x = year)) +
  geom_rect(data = events_all,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = as.factor(label)),
            alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = value, color = measure)) +
  geom_vline(data = events_hh, aes(xintercept = year),
             linetype = "dashed", linewidth = 0.3, colour = "#111111") +
  geom_text(data = events_hh,
            aes(x = year, label = label),
            y = 0.7, size = 2.5, colour = "#111111", inherit.aes = FALSE) +
  scale_x_continuous(guide  = guide_axis(minor.ticks = TRUE),
                     breaks = seq(1950, 2030, by = 20)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     guide  = guide_axis(minor.ticks = TRUE)) +
  scale_colour_paletteer_d("MoMAColors::Lupi", direction = -1) +
  scale_fill_manual(values = c("Reagan" = "#E5E5E5", "Thatcher" = "#E5E5E5")) +
  custom_theme() +
  facet_wrap(~ measure, scales = "free") +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL)

ggsave(plot = last_plot(), "./figures/debt_measures.png",
       dpi = 800, width = 9, height = 4.5, limitsize = FALSE)

# Coverage check
raw_ts |> 
  mutate(period = case_when(
    year < 1979 ~ "Pre 1979",
    year >= 1979 & year < 1982 ~ "Thatcher & Reagan Elections",
    year >= 1982 & year < 1991 ~ "Post 1981",
    year >= 1991 & year < 2001 ~ "Roaring Nineties",
    year >= 2001 & year < 2013 ~ "Great Recession",
    year >= 2013 & year <= 2023 ~ "Recent"
  )) |> 
  summarize(value = weighted.mean(value, n_countries), .by = c(period, measure))

measure_labels <- c(
  p90p100          = "Top 10% Income Share",
  p99p100          = "Top 1% Income Share",
  gini_mean        = "Gini Coefficient",
  public_debt      = "Public Debt",
  ggd_private_debt = "Private Debt",
  ggd_nfc_debt     = "Non-Financial Corporate Debt",
  ggd_hh_debt      = "Household Debt"
)

income_levels <- c("Low income", "Lower middle income", "Upper middle income", "High income")

long <- raw |>
  filter(!is.na(income_group)) |>
  mutate(gini_mean = gini_mean / 100) |>
  select(iso, year, income_group, rgdpo, all_of(names(measure_labels))) |>
  pivot_longer(all_of(names(measure_labels)), names_to = "measure", values_to = "value") |>
  filter(!is.na(value), !is.na(rgdpo)) |>
  arrange(measure, income_group, iso, year)

# Chained group series: year-over-year changes among countries observed in
# consecutive years, cumulated and anchored to the final-year cross-section
changes <- long |>
  mutate(
    d_value = value - dplyr::lag(value),
    consec  = dplyr::lag(year) == year - 1,
    .by = c(measure, income_group, iso)
  ) |>
  filter(consec, !is.na(d_value)) |>
  summarize(
    d_value = weighted.mean(d_value, rgdpo),
    n_countries = n_distinct(iso),
    .by = c(measure, income_group, year)
  )

anchors <- changes |>
  slice_max(year, by = c(measure, income_group)) |>
  select(measure, income_group, anchor_year = year) |>
  left_join(long, by = join_by(measure, income_group, anchor_year == year)) |>
  summarize(anchor_value = weighted.mean(value, rgdpo),
            .by = c(measure, income_group, anchor_year))

group_ts <- changes |>
  arrange(measure, income_group, year) |>
  mutate(cum = cumsum(d_value), .by = c(measure, income_group)) |>
  left_join(anchors, by = c("measure", "income_group")) |>
  mutate(value = anchor_value + cum - last(cum), .by = c(measure, income_group)) |>
  select(measure, income_group, year, value, n_countries) |>
  mutate(
    measure = measure_labels[measure],
    income_group = factor(income_group, levels = income_levels)
  ) |>
  arrange(measure, income_group, year)

events_all <- tibble(
  xmin  = c(1979, 1981),
  xmax  = c(1990, 1989),
  label = c("Thatcher", "Reagan")
)

inequality_measures <- c("Top 10% Income Share", "Top 1% Income Share")
debt_measures <- c("Public Debt", "Private Debt", "Non-Financial Corporate Debt", "Household Debt")

chain_caption <- "Shaded areas denote the administrations of Margaret Thatcher (UK, 1979-1990) and Ronald Reagan (US, 1981-1989)."

# Inequality measures by income group
group_ts |>
  filter(measure %in% inequality_measures) |>
  ggplot(aes(x = year)) +
  geom_rect(data = events_all,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = as.factor(label)),
            alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = value, color = income_group)) +
  scale_x_continuous(guide  = guide_axis(minor.ticks = TRUE),
                     breaks = seq(1950, 2020, by = 20)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     guide  = guide_axis(minor.ticks = TRUE)) +
  scale_colour_paletteer_d("MoMAColors::Lupi", direction = -1) +
  scale_fill_manual(values = c("Reagan" = "#E5E5E5", "Thatcher" = "#E5E5E5")) +
  custom_theme() +
  facet_wrap(~ measure, scales = "free") +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL)

ggsave(plot = last_plot(), "./figures/inequality_measures.png",
       dpi = 600, width = 7, height = 3.4, limitsize = FALSE)

# Debt measures by income group
group_ts |>
  filter(measure %in% debt_measures) |>
  ggplot(aes(x = year)) +
  geom_rect(data = events_all,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = as.factor(label)),
            alpha = 0.35, inherit.aes = FALSE) +
  geom_line(aes(y = value, color = income_group)) +
  scale_x_continuous(guide  = guide_axis(minor.ticks = TRUE),
                     breaks = seq(1950, 2030, by = 20)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     guide  = guide_axis(minor.ticks = TRUE)) +
  scale_colour_paletteer_d("MoMAColors::Lupi", direction = -1) +
  scale_fill_manual(values = c("Reagan" = "#E5E5E5", "Thatcher" = "#E5E5E5")) +
  custom_theme() +
  facet_wrap(~ measure, scales = "free") +
  guides(fill = "none") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL, caption = chain_caption)

ggsave(plot = last_plot(), "./figures/debt_measures_income_group.png",
       dpi = 800, width = 9, height = 5, limitsize = FALSE)

# Decade means by income group
decade_means <- group_ts |>
  mutate(decade = floor(year / 10) * 10) |>
  summarize(value = mean(value), .by = c(measure, income_group, decade)) |>
  arrange(measure, income_group, decade) |>
  pivot_wider(names_from = decade, values_from = value, names_prefix = "d")

write_csv(decade_means, "./tables/decade_means_income_group.csv")

decade_means |>
  mutate(across(starts_with("d"), ~ if_else(is.na(.x), "", scales::percent(.x, accuracy = 0.1)))) |>
  rename_with(~ str_remove(.x, "^d"), starts_with("d")) |>
  rename(Measure = measure, `Income group` = income_group) |>
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "",
               caption = "Decade means by modal World Bank income classification. Chained GDP-weighted cross-country means.",
               label = "tab:decade_means_income_group")
# Linear trends by income group
trends <- group_ts |>
  summarize(
    first_year   = min(year),
    last_year    = max(year),
    start_value  = value[year == min(year)],
    end_value    = value[year == max(year)],
    total_change = end_value - start_value,
    trend_decade = coef(lm(value ~ year))[2] * 10,
    trend_se     = summary(lm(value ~ year))$coefficients[2, 2] * 10,
    .by = c(measure, income_group)
  ) |>
  arrange(measure, income_group)

write_csv(trends, "./tables/trends_income_group.csv")

trends |>
  transmute(
    Measure        = measure,
    `Income group` = income_group,
    Span           = str_c(first_year, "--", last_year),
    Start          = scales::percent(start_value, accuracy = 0.1),
    End            = scales::percent(end_value, accuracy = 0.1),
    `Total change` = scales::percent(total_change, accuracy = 0.1),
    `Trend per decade` = str_c(scales::percent(trend_decade, accuracy = 0.01),
                               " (", scales::percent(trend_se, accuracy = 0.01), ")")
  ) |>
  knitr::kable(format = "latex", booktabs = TRUE, linesep = "",
               caption = "Linear trends by modal World Bank income classification. Trend per decade is the OLS slope of the chained group series on year, scaled to a ten-year change, with its standard error in parentheses.",
               label = "tab:trends_income_group")

trends