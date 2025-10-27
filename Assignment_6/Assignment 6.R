library(tidyverse)
library(gganimate)


#load and tidy data
dat_raw <- read_csv("../Data/BioLog_Plate_Data.csv", show_col_types = FALSE)

dat_long <- dat_raw |>
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance",
    names_prefix = "Hr_"
  ) |>
  mutate(
    Time = as.numeric(Time),
    Dilution = as.numeric(Dilution),
    Rep = as.factor(Rep)
  ) |>
  mutate(
    Type = case_when(
      str_detect(tolower(`Sample ID`), "soil")  ~ "Soil",
      str_detect(tolower(`Sample ID`), "water") ~ "Water",
      TRUE ~ "Other"   
    )
  )


dat_01_mean <- dat_long |>
  filter(Type %in% c("Soil", "Water"),
         abs(Dilution - 0.1) < 1e-9) |>
  group_by(Type, Substrate, Time) |>
  summarise(Absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")

plot_facets <- ggplot(dat_01_mean, aes(Time, Absorbance, color = Type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4, alpha = 0.9) +
  facet_wrap(~ Substrate, ncol = 6) +
  scale_y_continuous(limits = c(0, 2.0)) +
  labs(title = "BioLog: Dilution 0.1", x = "Time (hours)", y = "Absorbance") +
  theme_light(base_size = 12) +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 9))

dir.create("figs", showWarnings = FALSE)
ggsave("figs/dilution_0.1_facets.png", plot_facets, width = 14, height = 7, dpi = 300)

#gif
itaconic_means_by_sample <- dat_long |>
  filter(Substrate == "Itaconic Acid") |>
  group_by(`Sample ID`, Dilution, Time) |>
  summarise(Mean_absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop") |>
  mutate(
    Dilution_f = factor(
      Dilution,
      levels = c(0.001, 0.01, 0.1),
      labels = c("0.001", "0.01", "0.1")
    )
  )

anim_itaconic <- ggplot(
  itaconic_means_by_sample,
  aes(x = Time, y = Mean_absorbance, color = `Sample ID`, group = `Sample ID`)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Dilution_f, nrow = 1) +
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(x = "Time", y = "Mean_absorbance") +
  theme_light(base_size = 12) +
  theme(panel.grid.minor = element_blank()) +
  transition_reveal(along = Time)

anim_save(
  "figs/itaconic_mean_by_sample.gif",
  animate(anim_itaconic, width = 640, height = 400, res = 120)
)

