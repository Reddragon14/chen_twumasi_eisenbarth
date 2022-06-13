df <- read_csv("./GitHub/chen_twumasi_eisenbarth/data/merged_data.csv")

df <- mutate(df, region = if_else(country == "Cote d'Ivoire", "Sub-Saharan Africa", region))

th <- theme_clean() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.background = element_rect(color = NA),
    legend.position = "bottom",
    axis.line = element_line(size = 0.25, colour = "darkgrey"),
    legend.title = element_text(size = 10, face = "plain"),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "transparent", color = NA),
    axis.ticks.length = unit(.35, "cm")
  )

ggplot(df, aes(x = pvd,
               y = cg,
               size  = pop, 
               color = factor(region))) +
  geom_jitter() +
  scale_size_continuous(range = c(1, 15),
                        labels = comma_format(),
                        breaks = c(5e6, 5e7, 1e8, 1e9)) +
  th +
  guides(size = guide_legend(override.aes = list(color = spanish_pink))) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = c(grape, raspberry, azure, prussian, dark_emerald, sugar_plum, spanish_blue, spanish_pink)) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       color = "",
       size = "Population"
    )

ggplot(
  df %>% group_by(country) %>% summarize(
    pvd = mean(pvd, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = pvd,
      y = cg,
      size  = pop)
) +
  geom_jitter(color = sapphire) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  th +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = c(grape, raspberry, prussian)) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")

ggplot(
  df %>% group_by(country) %>% summarize(
    rgdpo = mean(rgdpo, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = rgdpo,
      y = cg,
      size  = pop, )
) +
  geom_jitter(color = sapphire) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  th +
  scale_x_log10(guide = "axis_minor", labels = dollar_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = c(grape, raspberry, prussian)) +
  labs(y = "Total government debt\n",
       x = "\nPrivate sector debt",
       fill = "",
       size = "Population")
  

ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    labsh = mean(labsh, na.rm = TRUE),
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = labsh,
      y = cg,
      color = lmh,
      size  = pop)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  th +
  guides(size = NULL) +
  scale_color_manual(values = c(grape, dark_emerald, raspberry, sapphire)) +
  scale_x_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  labs(y = "Total government debt\n",
       x = "\nLabor share",
       color = "",
       size = "Population")

ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    cg = mean(cg, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(
    x = lmh,
    y = cg,
    color = lmh,
    size  = pop
  )
) +
  geom_jitter(width = 0.1) +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  th +
  guides(size = guide_legend(override.aes = list(color = lilac), nrow = 2)) +
  scale_y_continuous(guide = "axis_minor", labels = percent_format()) +
  scale_color_manual(values = c(rain_day, dark_emerald, raspberry, sapphire)) +
  labs(
    y = "Total government debt\n",
    x = "",
    fill = "",
    color = "",
    size = "Population"
  )


ggplot(
  df %>% group_by(country, lmh) %>% summarize(
    life_exp = mean(life_exp, na.rm = TRUE),
    gni = mean(real_gni, na.rm = TRUE),
    pop = max(pop, na.rm = TRUE)
  ),
  aes(x = gni,
      y = life_exp,
      color = lmh,
      size  = pop)
) +
  geom_jitter() +
  scale_size_continuous(
    range = c(1, 15),
    labels = comma_format(),
    breaks = c(5e6, 5e7, 1e8, 1e9)
  ) +
  th +
  guides(size = guide_legend(override.aes = list(color = lilac), nrow = 2)) +
  scale_color_manual(values = c(grape, dark_emerald, raspberry, sapphire)) +
  scale_x_log10(guide = "axis_minor", labels = dollar_format()) +
  scale_y_continuous(guide = "axis_minor", labels = number_format(), limits = c(40, 80)) +
  labs(y = "Life expectancy\n",
       x = "\nGDP-per-capita",
       color = "",
       size = "Population")
