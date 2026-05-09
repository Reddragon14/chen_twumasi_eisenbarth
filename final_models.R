library(tidyverse)
library(estimatr)
library(stargazer)
library(plm)

dataset = read_csv("updated_dataset.csv")

dataset |>
  group_by(income_group) |>
  distinct(country) -> countries

ns <-
dataset |>
  summarize(time_observations = n(), .by = country) |>
  arrange(-time_observations)

p_df <-
  plm::pdata.frame(
    dataset,
    index = c("country", "year"),
    drop.index = TRUE,
    row.names = TRUE
  )

upper_mid <- filter(dataset, income_group == "Upper middle income")
high <- filter(dataset, income_group == "High income")
low <- filter(dataset, income_group == "Low income")
lower_mid <- filter(dataset, income_group == "Lower middle income")


summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = upper_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = high, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = low, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))
summary(lm_robust(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = lower_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population, clusters = country))



fit1 <- lm(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)
fit2 <-  lm(log(gini_imputed) ~ debt_imp + gdp +  + gdp*debt_imp + hc, data = upper_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)
fit3 <-  lm(log(gini_imputed) ~ debt_imp + gdp +  gdp*debt_imp + hc, data = high, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)
fit4 <-  lm(log(gini_imputed) ~ debt_imp + gdp  + gdp*debt_imp + hc, data = low, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)
fit5 <- lm(log(gini_imputed) ~ debt_imp + gdp +  gdp*debt_imp + hc, data = lower_mid, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)
fit6 <- lm(log(gini_imputed) ~ debt_imp + gdp + gdp*debt_imp + hc, data = dataset, fixed_effects =  ~ country + year, se_type = "stata", weights =  population)

stargazer(fit1, fit2, fit3, fit4, fit5)

glance(fit1) |> mutate(model = "Pooled")

model1 <- tidy(fit1) |> mutate(model = "Pooled")
model2 <- tidy(fit2) |> mutate(model = "Upper Middle")
model3 <- tidy(fit3) |> mutate(model = "High")
model4 <- tidy(fit4) |> mutate(model = "Low")
model5 <- tidy(fit5) |> mutate(model = "Lower Middle")
model6 <- tidy(fit6) |> mutate(model = "Pooled (ARDL)")

report(fit1)

coefs <- bind_rows(model1, model2, model3, model4, model5, model6)

write_csv(coefs, "model_coefs.csv")

ts <- zoo::zooreg(data = full_data, start = 1973, end = 2019)

models <- ARDL::auto_ardl(gdp ~ debt_imp, data = ts, max_order = 5)
models$best_order
models$best_model