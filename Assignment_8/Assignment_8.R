library(tidyverse)

mush <- read_csv("mushroom_growth.csv", show_col_types = FALSE)

#exploratory plots
mush %>% 
  pivot_longer(c(Light, Nitrogen, Temperature),
               names_to = "Predictor", values_to = "Value") %>%
  ggplot(aes(Value, GrowthRate)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Predictor, scales = "free_x") +
  theme_minimal()

mush %>% 
  pivot_longer(c(Species, Humidity),
               names_to = "Predictor", values_to = "Category") %>%
  mutate(Category = as.factor(Category)) %>%
  ggplot(aes(Category, GrowthRate)) +
  geom_boxplot() +
  facet_wrap(~ Predictor, scales = "free_x") +
  theme_minimal()

#models
mod1 <- lm(GrowthRate ~ 1, data = mush)
mod2 <- lm(GrowthRate ~ Light, data = mush)
mod3 <- lm(GrowthRate ~ Light + Nitrogen, data = mush)
mod4 <- lm(GrowthRate ~ Light + Nitrogen + Temperature + Species + Humidity, data = mush)

#mse comparison
mse_tbl <- tibble(
  model = c("mod1", "mod2", "mod3", "mod4"),
  mse = c(
    mean(residuals(mod1)^2),
    mean(residuals(mod2)^2),
    mean(residuals(mod3)^2),
    mean(residuals(mod4)^2)
  )
) %>% arrange(mse)

mse_tbl

best <- mse_tbl$model[1]
best_model <- get(best)

summary(best_model)

#predictions for real data
mush$Predicted <- predict(best_model, newdata = mush)
mush$Type <- "Real"

#hypothetical predictions
light_range <- range(mush$Light, na.rm = TRUE)

hypo <- tibble(
  Light = seq(light_range[1], light_range[2], length.out = 50),
  Nitrogen = median(mush$Nitrogen),
  Temperature = median(mush$Temperature),
  Species = mush$Species[1],
  Humidity = mush$Humidity[1]
)

hypo$Predicted <- predict(best_model, newdata = hypo)
hypo$Type <- "Hypothetical"

#plot predictions
ggplot() +
  geom_point(data = mush, aes(Light, GrowthRate), alpha = 0.6) +
  geom_line(data = hypo, aes(Light, Predicted, color = Type), linewidth = 1) +
  theme_minimal()

#nonlinear data set example
nonlin <- read_csv("non_linear_relationship.csv", show_col_types = FALSE)
nonlin_lm <- lm(response ~ predictor, data = nonlin)
summary(nonlin_lm)

hypo
summary(hypo$Predicted)
