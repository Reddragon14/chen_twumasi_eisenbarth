
wid_data_p99 <- read_excel("C:/Users/ageis/OneDrive/Documents/WID_Data_29032026-100548.xlsx") |> 
  select(-indicator)

wid_data_p90 <- read_excel("C:/Users/ageis/OneDrive/Documents/WID_Data_29032026-100423.xlsx") |> 
  select(-indicator)

wid_data <- left_join(wid_data_p99, wid_data_p90) |> 
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Cote d'Ivoire",
    country == "USA" ~ "United States",
    .default = country
  ))

pwt_names <- distinct(pwt, country) |> mutate(PWT = TRUE)

wid_country_names <- distinct(wid_data, country) |> mutate(WIID = TRUE) |> 
  mutate(country = case_when(
    country == "Cote d'Ivoire" ~ "Cote d'Ivoire",
    country == "USA" ~ "United States",
    .default = country
  ))

pwt <- 
  pwt |> 
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)"   ~ "Bolivia",
    country == "China, Hong Kong SAR"               ~ "Hong Kong",
    country == "China, Macao SAR"                   ~ "Macao",
    country == "Côte d'Ivoire"                      ~ "Cote d'Ivoire",
    country == "Curaçao"                            ~ "Curacao",
    country == "Czechia"                            ~ "Czech Republic",
    country == "D.R. of the Congo"                  ~ "DR Congo",
    country == "Eswatini"                           ~ "Swaziland",
    country == "Iran (Islamic Republic of)"         ~ "Iran",
    country == "Lao People's DR"                    ~ "Lao PDR",
    country == "Republic of Korea"                  ~ "Korea",
    country == "Republic of Moldova"                ~ "Moldova",
    country == "St. Vincent and the Grenadines"     ~ "Saint Vincent and the Grenadines",
    country == "State of Palestine"                 ~ "Palestine",
    country == "Türkiye"                            ~ "Turkey",
    country == "U.R. of Tanzania: Mainland"         ~ "Tanzania",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    .default = country
  )) 

joined_names <- pwt_names |> 
  inner_join(wid_country_names)
