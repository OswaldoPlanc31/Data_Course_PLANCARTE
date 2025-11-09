#install packages
library(tidyverse)

#importing the csv file
religion_data <- read_csv("../Data/Utah_Religions_by_County.csv", show_col_types = FALSE)

#taking a peek at the data

glimpse(religion_data)

#making folders for outputs and bypassing warning message
dir.create("figs", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

#making the data tidy but keeping county and pop2010 as identifiers, making all columns long
religion_long <- religion_data |>
  pivot_longer(
    cols = -c(County, Pop_2010),
    names_to = "Religion",
    values_to = "Proportion"
  )

#looking at the new tidy data
head(religion_long)

#Question #1: “Does population of a county correlate with the proportion of any specific religious group in that county?”

#plotting population vs. proportion
# ---- 5) Plot: Population vs. Proportion ----
p1 <- ggplot(religion_long, aes(x = Pop_2010, y = Proportion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(
    title = "County Population vs. Religious Proportion",
    x = "Population (2010)",
    y = "Proportion of County"
  ) +
  theme_minimal()

#saving the plot to figs folder
ggsave("figs/population_vs_religion.png", p1, width = 12, height = 8, dpi = 300)


#Question #2: “Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”

#getting the non religious columns
nonrel_df <- religion_data |>
  select(County, NonReligious = `Non-Religious`)

#joining and removing the non religious rows
rel_vs_nonrel <- religion_long |>
  filter(Religion != "Non-Religious") |>
  left_join(nonrel_df, by = "County")

#making the scatter plot for religion vs. non religious
p2 <- ggplot(rel_vs_nonrel, aes(x = Proportion, y = NonReligious)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "maroon") +
  facet_wrap(~ Religion, scales = "free") +
  labs(
    title = "Religion Proportion vs Non-Religious (by County)",
    x = "Religion Proportion",
    y = "Non-Religious Proportion"
  ) +
  theme_minimal()

#saving the plot to figs folder
ggsave("figs/religion_vs_nonreligious.png", p2, width = 12, height = 8, dpi = 300)

