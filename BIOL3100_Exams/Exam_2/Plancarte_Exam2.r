library(tidyverse)


unicef <- read.csv("unicef-u5mr.csv")
glimpse(unicef)


unicef_tidy <- unicef |>
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    values_to = "U5MR",
    names_prefix = "U5MR."
  ) |>
  mutate(
    Year = as.numeric(Year)
  )

glimpse(unicef_tidy)


p1 <- unicef_tidy |>
  ggplot(aes(x = Year, y = U5MR,
             group = CountryName,
             color = CountryName)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Continent) +
  labs(
    title = "Under-5 Mortality Rate by Country and Continent",
    x = "Year",
    y = "U5MR, deaths per 1000 live births"
  ) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(
  filename = "Plancarte_Plot_1.png",
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)


continent_mean <- unicef_tidy |>
  group_by(Continent, Year) |>
  summarize(
    mean_U5MR = mean(U5MR, na.rm = TRUE)
  )


p2 <- continent_mean |>
  ggplot(aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1.1) +
  labs(
    title = "Mean Under-5 Mortality Rate by Continent",
    x = "Year",
    y = "Mean U5MR, deaths per 1000 live births"
  ) +
  theme_bw()

ggsave(
  filename = "Plancarte_Plot_2.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

#models
mod1 <- lm(U5MR ~ Year, data = unicef_tidy)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

model_compare <- AIC(mod1, mod2, mod3)
print(model_compare)

# AIC comparison
# model1 AIC = 117100.7
# model2 AIC = 110281.1
# model3 AIC = 109215.1
# model 3 has the lowest AIC, so it is the best performing model.

prediction_grid <- unicef_tidy |>
  distinct(Continent, Year) |>
  arrange(Continent, Year)

prediction_grid <- prediction_grid |>
  mutate(
    pred_mod1 = predict(mod1, newdata = prediction_grid),
    pred_mod2 = predict(mod2, newdata = prediction_grid),
    pred_mod3 = predict(mod3, newdata = prediction_grid)
  )

prediction_long <- prediction_grid |>
  pivot_longer(
    cols = starts_with("pred_mod"),
    names_to = "Model",
    values_to = "pred"
  ) |>
  mutate(
    Model = case_when(
      Model == "pred_mod1" ~ "mod1",
      Model == "pred_mod2" ~ "mod2",
      Model == "pred_mod3" ~ "mod3",
      TRUE ~ Model
    )
  )


p3 <- prediction_long |>
  ggplot(aes(x = Year, y = pred, color = Continent)) +
  geom_line() +
  facet_wrap(~ Model) +
  labs(
    title = "Model Predictions of U5MR by Year and Continent",
    x = "Year",
    y = "Predicted U5MR"
  ) +
  theme_bw()

print(p3)


















