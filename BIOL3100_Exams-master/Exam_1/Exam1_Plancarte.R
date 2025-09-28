library(tidyverse)

covid_data <- readr::read_csv("cleaned_covid_data.csv", show_col_types = FALSE)
glimpse(covid_data)

A_states <- covid_data %>%
  filter(stringr::str_starts(Province_State, "A"))

A_states %>% distinct(Province_State) %>% arrange(Province_State)

p_A_deaths <- ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.4, size = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "Deaths over time, States starting with 'A'",
       x = "Date", y = "Deaths") +
  theme_minimal(base_size = 12)

p_A_deaths

state_max_fatality_rate <- covid_data %>%
  filter(!is.na(Case_Fatality_Ratio)) %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)

p_max_fatality <- ggplot(
  state_max_fatality_rate %>%
    mutate(Province_State = forcats::fct_reorder(Province_State, Maximum_Fatality_Ratio, .desc = TRUE)),
  aes(x = Province_State, y = Maximum_Fatality_Ratio)
) +
  geom_col() +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "State", y = "Max CFR (%)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p_max_fatality

us_deaths <- covid_data %>%
  group_by(Last_Update) %>%
  summarise(US_Cumulative_Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(Last_Update)

ggplot(us_deaths, aes(x = Last_Update, y = US_Cumulative_Deaths)) +
  geom_line() +
  labs(title = "US Cumulative COVID-19 Deaths Over Time",
       x = "Date", y = "Cumulative deaths") +
  theme_minimal(base_size = 12)

