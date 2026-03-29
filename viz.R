library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(ggthemes)
library(scales)
library(extrafont)
library(ggpubr)

sapphire <- "#255F85"
lapis <- "#26619c"
classic_blue <- "#0F4C81"
prussian <- "#293E66"
rainy_day <- "#474C5c"
glossy_grape <- "#A799B7"
raspberry <- "#C33149"
saffron <- "#F49D37"
dark_violet <- "#351C45"
dark_magenta <- "#861B54"
dark_emerald <- "#1F7A69"
satin <- "#D65C70"
onyx <- "#383C42"
rhythm <- "#6E758E"

ggthemr::ggthemr(palette = 'greyscale', layout = "clean")


wb_classes <- read_csv("./data/wb_classes.csv") |> janitor::clean_names() |>
  select(-economy)

countrycodes |>
  left_join(wb_classes, by = join_by(countrycode == code))

dataset <- read_csv("final_dataset.csv")

dataset <- mutate(df, region = if_else(country == "Cote d'Ivoire", "Sub-Saharan Africa", region))

mutate(dataset, lmh = str_to_title(income_group)) |> 
  distinct(income_group)

dataset <-
  mutate(dataset, income_group = str_to_title(income_group) %>% factor(
    .,
    levels = c(
      "Low Income",
      "Lower Middle Income",
      "Upper Middle Income",
      "High Income"
    )
  ))

# dataset <-
#   mutate(dataset, lmh = str_to_title(lmh) %>% factor(
#     .,
#     levels = c(
#       "Low Income",
#       "Lower-Middle Income",
#       "Upper-Middle Income",
#       "High Income"
#     )
#   ))


defacto <- readxl::read_xlsx("./data/defacto_regimes.xlsx")
dejure <- readxl::read_xlsx("./data/dejure_regimes.xlsx")


# grid style ggplot theme
theme_grid <- function(...) {
  theme_light() +
    theme(
      legend.background = element_rect(color = NA),
      legend.position = "bottom",
      legend.key = element_rect(fill = NA),
      panel.border = element_blank(),
      legend.title = element_text(size = 10, face = "plain"),
      text = element_text(family = "Arial", color = "darkgrey"),
      plot.background = element_rect(fill = "transparent", color = NA),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(colour = 'black')
    )
}

ggplot(dataset, aes(
  x = pvd,
  y = cg,
  size  = pop,
  color = factor(region)
)) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  guides(size = guide_legend(override.aes = list(color = glossy_grape), nrow = 2)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(
    y = "Total government debt\n",
    x = "\nPrivate sector debt",
    fill = "",
    color = "",
    size = "Population"
  )


dataset %>% group_by(country, income_group) %>% summarize(
  pvd = mean(pvd, na.rm = TRUE),
  cg = mean(cg, na.rm = TRUE),
  pop = max(pop, na.rm = TRUE)
) %>%
  ggplot(aes(
    x = pvd,
    y = cg,
    size  = pop,
    color = income_group
  )) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = wesanderson::wes_palette("AsteroidCity3")) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")

ggplot(
  dataset %>% group_by(country, income_group) %>% summarize(
    rgdpo = mean(rgdpo, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = rgdpo,
      y = cg,
      size  = pop,
      color = income_group)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_x_log10(guide = "axis_minor", labels = dollar_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = wesanderson::wes_palette("AsteroidCity3")) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")
  

ggplot(
  dataset %>% group_by(country, income_group) %>% summarize(
    labsh = mean(labsh, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = labsh,
      y = cg,
      color = income_group,
      size  = pop)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  guides(size = guide_legend(override.aes = list(color = lapis), nrow = 2)) +
  scale_color_manual(values = wesanderson::wes_palette(name = "AsteroidCity1")) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(y = "Total government debt\n",
       x = "\nLabor share",
       color = "",
       size = "Population")


guides(
  size = guide_legend(
    override.aes = list(color = "grey70"),
    theme = theme(
      legend.title.position = "top",
      legend.text.position = "bottom",
      legend.title = element_text(hjust = 0.5)
    ),
    nrow = 2
  ),
  color = guide_legend(nrow = 2)
) 


gross_debt <- dataset |>
  ungroup() |>
  summarize(
    cg = mean(debt_imp, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE),
    .by = c(country, income_group)
  )

global_avg <- mean(gross_debt$global)


group_averages <- debt_summary |>
  summarize(group_avg = mean(cg, na.rm = TRUE), .by = income_group)

ggplot(gross_debt,
       aes(
         x = income_group,
         y = cg,
         color = income_group,
         size = pop
       )) +
  # Jittered country-level points
  geom_jitter(width = 0.1, alpha = 0.7) +
  
  # Bar-like group average lines
  geom_crossbar(
    data = group_averages,
    aes(
      x = income_group,
      y = group_avg,
      ymin = group_avg,
      ymax = group_avg
    ),
    color = "black",
    width = 0.5,
    fatten = 1,
    inherit.aes = FALSE
  ) +
  
  # Horizontal global average line
  geom_hline(
    yintercept = global_avg,
    linetype = "dotted",
    color = "gray40",
    linewidth = 0.8
  ) +
  
  # Aesthetic scaling
  scale_size_continuous(
    range = c(1, 8),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  scale_y_continuous(
    guide = "axis_minor",
    labels = percent_format(),
    breaks = seq(0, 1.5, by = 0.2),
    limits = c(0, 1.5)
  ) +
  scale_colour_paletteer_d("lisa::PabloPicasso_1") +
  labs(
    y = "Total government debt\n",
    x = "",
    fill = "",
    color = "",
    size = "Population"
  ) +
  theme(legend.position = "bottom") +
  guides(size = "none", color = guide_legend(nrow = 2))

ggsave("total_government_debt_icg.png", width = 7, height = 5, dpi = 1080, plot = last_plot())

dataset %>% summarize(
  gini = mean(gini, na.rm = TRUE),
  cg = mean(debt_imp, na.rm = TRUE),
  pop = max(pop, na.rm = TRUE),
  .by = c(country, income_group)) %>%
  ggplot(aes(x = cg, y = gini)) +
  geom_jitter(aes(color = income_group, size  = pop), width = 0.1) +
  stat_smooth(
    aes(x = cg, y = gini),
    linewidth = 0.75,
    se = FALSE,
    method = "lm"
  ) +
  scale_size_continuous(
    range = c(1, 7),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  guides(
    size = "none",
    color = guide_legend(nrow = 2)
  ) +
  scale_x_continuous(
    guide = "axis_minor",
    labels = percent_format(),
    breaks = seq(0, 1.3, by = 0.2),
    limits = c(0, 1.2)
  ) +
  scale_y_continuous(guide = "axis_minor", 
                     labels = number_format(),
                     breaks = seq(0, 100, by = 10),
                     limits = c(20, 70)) +
  scale_colour_paletteer_d("lisa::PabloPicasso_1") +
  theme(legend.position = "bottom") +
  labs(
    y = "Gini Coefficient\n",
    x = "Total government debt",
    fill = "",
    color = "",
    size = "Population"
  )

ggsave("latex/figure2.png", dpi = 1080, width = 7, height = 5)

final_countries <- dataset |> 
  distinct(country, income_group) 
  pivot_wider(names_from = income_group,
              values_from = country)

dataset |>
  distinct(country, region, income_group) |>
  summarize(n = n(), .by = c(region, income_group)) |>
  mutate(income_group = str_to_title(income_group)) |>
  pivot_wider(names_from = income_group, 
              values_from = n) |>
  mutate(across(2:5, ~ if_else(is.na(.x), 0, as.numeric(.x)))) |>
  mutate(Total = `Upper Middle Income` + `High Income` + `Low Income` + `Lower Middle Income`, .by = region) |>
  janitor::adorn_totals() 
  kable(booktabs = TRUE,  format = "latex") |>
  kable_classic_2()
  save_kable("dataset.tex", format = "latex")
