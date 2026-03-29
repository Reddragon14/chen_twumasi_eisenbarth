
gini <- haven::read_dta("C:/Users/ageis/Downloads/swiid9_6.dta") |>
  select(country, year, 4:103) |>
  pivot_longer(cols = 3:102) |>
  summarize(gini = mean(value), .by = c(country, year)) |>
  complete(country, year) |>
  mutate(
    country = case_when(
      country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
      country == "Congo-Kinshasa" ~ "Congo",
      country == "Korea" ~ "South Korea",
      TRUE ~ as.character(country)
    ))

missing_gini <-
  gini |>
  filter(year >= 1980) |>
  summarize(missing = sum(if_else(is.na(gini), 1, 0)), .by = country) |>
  arrange(-missing)

included <-
  missing_gini |>
  filter(missing <= 20) |>
  distinct(country) %>%
  as_vector()

final_selection <- intersect(included, final_selected_countries)

final_gini <- gini |> filter(country %in% final_selection, year >= 1973)

gini_countries <-
  final_gini |>
  distinct(country) |>
  mutate(match = TRUE) 

dataset_countries <-
  dataset |>
  distinct(country) |>
  mutate(inclued = TRUE) |>
  left_join(gini_countries) |>
  filter(!is.na(match)) |>
  distinct(country) |>
  as_vector()

write_csv(dataset_countries, "final_dataset_countries.csv")


wb_population <- read_csv("wb_population.csv") |>
  janitor::clean_names() |>
  select(country_code, 5:68) |>
  mutate(across(2:65, as.numeric)) |>
  pivot_longer(cols = 2:65) |>
  mutate(year = seq(1960, 2023, by = 1), .by = country_code) |>
  transmute(country_code, year, population = value)
