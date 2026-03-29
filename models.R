library(dplyr)
library(tidyverse)
library(tidyquant)
library(scales)
library(ggplot2)
library(distributional)
library(ggdist)
library(extrafont)
library(ggh4x)
library(ggthemes)
library(hrbrthemes)
library(fredr)

prussian <- "#293E66"
grape <- "#968AA8"
saffron <- "#F49D37"
magenta <- "#922D50"
sapphire <- "#255F85"
taupe <- "#715B5B"
rainy_day <- "#474C5c"
baby_pink <- "#EDBFC6"

theme_clean <- function(...) {
  ggpubr::theme_classic2() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 1),
      panel.border = element_blank(),
      axis.line = element_line(linewidth = 0.10, colour = "#181818"),
      legend.title = element_text(size = 10, face = "plain"),
      legend.key = element_rect(fill = NA, color = NA),
      text = element_text(color = onyx, family = "Arial Narrow", size = 10),
      strip.background = element_rect(color = NA),
      plot.background = element_rect(color = NA),
      axis.ticks.length = unit(0.25, "cm"),
      axis.ticks = element_line(linewidth = 0.10)
    )
}


imf_debt <- haven::read_dta("./data/imf_debt_data.dta")
chudik <- read_csv("./data/chudik_countries.csv")
wb_classes <- read_csv("./data/wb_classes.csv") %>% janitor::clean_names()

#' change ampersands to "and"
imf_debt <-
  mutate(imf_debt, country = gsub("&", "and", country)) |>
  mutate(
    #' this fixes the discrepancy between the two sources in terms of country names
    country = case_when(
      country == "C.A.R." ~ "Central African Republic",
      country == "Congo, Dem. Rep. of" ~ "Congo D.R.",
      country == "Cote D'Ivoire" ~ "Cote d'Ivoire",
      country == "Kyrgyz Republic" ~ "Kyrgyzstan",
      country == "S?o Tom? and Pr?ncipe" ~ "Sao Tome and Principe",
      country == "Slovak Republic" ~ "Slovakia",
      country == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis",
      country == "St. Lucia" ~ "Saint Lucia",
      country == "Taiwan Province of China" ~ "Taiwan",
      country == "U.A.E." ~ "United Arab Emirates",
      TRUE ~ country,
    )
  ) |>
  #' remove unnecessary strings after comma
  mutate(country = gsub("^(.*?),.*", "\\1", country)) |>
  #' trim whitespace
  mutate(country = trimws(country)) |>
  mutate(across(
    c(pvd_all, pvd_ls, hh_all, hh_ls, nfc_all, nfc_ls, ps, nfps, gg, cg),
    ~ .x / 100
  ))

tbl <-
  df |>
  mutate(growth = (rgdpo - lag(rgdpo)) / lag(rgdpo),
         l_growth = log(rgdpo/lag(rgdpo)), .by = country) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg)))

missing_debt <-
  tbl |>
  summarize(missing_obs_gg = sum(is.na(gg)),
            missing_obs_cg = sum(is.na(cg)),
            missing_obs_debt = sum(is.na(debt)),
            .by = c(country, iso, ifs))

excluded <- filter(missing_debt, missing_obs_debt > 20)
excluded_countries <- unique(excluded$country)

data_df <- filter(tbl, !country %in% c(excluded_countries)) |>
  mutate(
    growth = (gdp - lag(gdp)) / lag(gdp),
    l_growth = log(gdp / lag(gdp)),
    .by = country
  ) |>
  mutate(debt = if_else(is.na(gg), as.numeric(cg), as.numeric(gg))) |>
  mutate(
    debt_imp = imputeTS::na_kalman(debt, model = "auto.arima"),
    growth_imp = imputeTS::na_interpolation(growth),
    .by = country
  ) |>
  select(-region) %>%
  left_join(wb_classes, by = join_by(iso == code)) %>%
  relocate(c(region, income_group, lending_category), .after = country)

data_df |>
  select(year, country, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", raspberry), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "") +
  theme_clean()

chudik_countries <- unique(chudik$Country)

wb_gdp <- read_csv("./data/wb_gdp.csv") |> janitor::clean_names() |> filter(!is.na(country_code)) |>
  pivot_longer(cols = 3:66) |>
  mutate(year = seq(1960, 2023, by = 1), .by = country_code) |>
  transmute(country_code, year, gdp = as.numeric(value) / 100)


dataset <-
  full_data |>
  left_join(wb_gdp, join_by(iso == country_code, year == year)) |>
  filter(country %in% final_selection) %>%
  left_join(final_gini) %>%
  select(-population) %>%
  left_join(world_bank_data,  join_by("country" == "country", "year" == "year")) %>%
  mutate(income_group = if_else(country == "Venezuela", "Upper middle income", income_group)) %>%
  filter(year >= 1973)


gdp_missing <-
  dataset |>
  filter(year >= 1980) |>
  select(year, country, iso, gdp) |>
  summarize(missing = sum(if_else(is.na(gdp), 1, 0)), .by = c(country, iso)) |>
  arrange(-missing) |>
  filter(missing >= 5) |>
  distinct(iso) |>
  as_vector()

rgdpc_missing <-
  dataset |>
  filter(year >= 1980) |>
  select(year, country, iso, rgdpc) |>
  summarize(missing = sum(if_else(is.na(rgdpc), 1, 0)), .by = c(country, iso)) |>
  arrange(-missing) |>
  filter(missing >= 5) |>
  distinct(iso) |>
  as_vector()

dataset <-
  dataset |>
  filter(!iso %in% gdp_missing) |>
  filter(!iso %in% rgdpc_missing)
  filter(country %in% dataset_countries)

dataset <- mutate(dataset, rgdpc_imputed = imputeTS:::na_interpolation(rgdpc), .by = iso)
dataset <- mutate(dataset, gini_imputed = imputeTS:::na_interpolation(gini), .by = iso)
dataset <- mutate(dataset, gdp_imputed = imputeTS:::na_interpolation(gdp), .by = iso)


dataset <-
  dataset |>
  mutate(gdp_capita = (rgdpc - lag(rgdpc, 1)) / lag(rgdpc, 1),
         .by = iso)

chudik_df <- filter(dataset, country %in% chudik_countries, year >= 1980)

chudik_df |>
  select(year, country, debt, debt_imp) |>
  pivot_longer(cols = c(debt, debt_imp)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c("darkgrey", "red"), labels = c("Original", "Imputed")) +
  labs(x = "Debt", y = "Density", color = "")
  theme_clean()

dataset |>
  filter(year >= 1960) |>
  select(year, country, gdp, gdp_capita) |>
  pivot_longer(cols = c(gdp, gdp_capita)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::percent_format(), guide = "axis_minor") +
  scale_color_manual(values = c(onyx, saffron), labels = c("Original", "Imputed")) +
  labs(x = "GDP Per-Capita", y = "Density", color = "")


monetary <- defacto |>
  select(year,
         country,
         iso,
         independence_dummy,
         independence_year,
         us_pegger_dummy)

dataset <-
  dataset |>
  left_join(monetary) |>
  filter(year >= 1973)

dataset <- mutate(
  dataset,
  gini_imputed = imputeTS::na_kalman(
    gini,
    model = "StructTS",
    smooth = TRUE,
    type = "trend"
  ),
  .by = country
)

dataset |>
  filter(year >= 1973) |>
  select(year, country, gini, gini_imputed) |>
  pivot_longer(cols = c(gini, gini_imputed)) |>
  ggplot(aes(x = value, color = name)) +
  stat_density(geom = "line", position = "identity") +
  scale_x_continuous(labels = scales::number_format(), guide = "axis_minor") +
  labs(x = "Gini", y = "Density", color = "")

dataset <-
  dataset |>
  select(-population) |>
  left_join(wb_population, join_by(iso == country_code, year == year))

write_csv(dataset, "final_dataset_revised.csv")

#dataset <- read_csv("final_dataset_revised.csv")


upper_mid <- filter(dataset, income_group == "Upper middle income")
high <- filter(dataset, income_group == "High income")
low <- filter(dataset, income_group == "Low income")
lower_mid <- filter(dataset, income_group == "Lower middle income")

summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed * debt_imp, data = dataset, fixed_effects = ~ country + year, se_type = "stata", weights = population_total, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed * debt_imp, data = upper_mid, fixed_effects = ~ country + year, se_type = "stata", weights = population_total, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed * debt_imp, data = high, fixed_effects = ~ country + year, se_type = "stata", weights = population_total, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed * debt_imp, data = low, fixed_effects = ~ country + year, se_type = "stata", weights = population_total, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed * debt_imp, data = lower_mid, fixed_effects = ~ country + year, se_type = "stata", weights = population_total, clusters = country))

fit1 <- lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed*debt_imp, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population_total, clusters = country)
fit2 <- lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed*debt_imp, data = upper_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population_total, clusters = country)
fit3 <- lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed*debt_imp, data = high, fixed_effects =  ~ country + year, se_type = "stata", weights =  population_total, clusters = country)
fit4 <- lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed*debt_imp, data = low, fixed_effects =  ~ country + year, se_type = "stata", weights =  population_total, clusters = country)
fit5 <- lm_robust(log(gini_imputed) ~ debt_imp + gdp_imputed + hc + gdp_imputed*debt_imp, data = lower_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population_total, clusters = country)


fit1 <- lm_robust(gini ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy , data = dataset, fixed_effects =  ~ country + year)
fit2 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = upper_mid, fixed_effects =  ~ country + year)
fit3 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = high, fixed_effects =  ~ country + year)
fit4 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = low, fixed_effects =  ~ country + year)
fit5 <- lm_robust(gdp ~ debt_imp + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = lower_mid, fixed_effects =  ~ country + year)
fit6 <- lm_robust(gdp ~ +lag(gdp) + debt_imp + lag(debt_imp) + pl_x + pl_m + independence_dummy + us_pegger_dummy, data = full_df, fixed_effects =  ~ country + year)


glance(fit1) |> mutate(model = "Pooled")

model1 <- tidy(fit1) |> mutate(model = "Pooled")
model2 <- tidy(fit2) |> mutate(model = "Upper Middle")
model3 <- tidy(fit3) |> mutate(model = "High")
model4 <- tidy(fit4) |> mutate(model = "Low")
model5 <- tidy(fit5) |> mutate(model = "Lower Middle")
model6 <- tidy(fit6) |> mutate(model = "Pooled (ARDL)")

coefs <- bind_rows(model1, model2, model3, model4, model5, model6)

write_csv(coefs, "model_coefs.csv")

ts <- zoo::zooreg(data = full_data, start = 1973, end = 2019)

models <- ARDL::auto_ardl(gdp ~ debt_imp, data = ts, max_order = 5)
models$best_order
models$best_model


summary(plm(growth ~ debt_imp + pl_x + pl_m, data = p_df, model = "within", effect = "twoways"))

g <- plm(inv ~ value + capital, data = Grunfeld, index = c("firm", "year"))
pcdtest(g)


summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + pl_x + pl_m, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))

