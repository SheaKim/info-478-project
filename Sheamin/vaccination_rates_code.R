# Dataset #1: Monthly Flu Vaccinations

df1 <- read.csv("monthly_cumulative.csv")

# Define the correct month order
month_levels <- c("SEP","OCT","NOV","DEC","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG")

# Step 1: Ensure month is a factor for proper sorting
df1 <- df1 %>%
  mutate(month = factor(month, levels = month_levels))

# getting new dose numbers
rates_df <- df1 %>%
  arrange(current_season, jurisdiction, age_group_label, month) %>%
  group_by(current_season, jurisdiction, age_group_label) %>%
  mutate(
    new_doses = numerator - lag(numerator, default = NA)  # New doses = current - previous
  ) %>%
  ungroup()

# summary table
monthly_trends <- rates_df %>%
  group_by(current_season, month) %>%
  summarise(new_doses = sum(new_doses, na.rm = TRUE)) %>%
  arrange(current_season, month)

print(monthly_trends)


# plot
rates_df %>%
  filter(!is.na(estimate)) %>%
  group_by(jurisdiction) %>%
  summarise(avg_vaccination_rate = mean(estimate, na.rm = TRUE)) %>%
  top_n(10, avg_vaccination_rate) %>%
  ggplot(aes(x = reorder(jurisdiction, avg_vaccination_rate), y = avg_vaccination_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Jurisdictions by Avg. Flu Vaccination Rate",
       x = "Jurisdiction", y = "Avg. Vaccination Rate") +
  theme_minimal()


# getting vaccination rates (new doses/population)
overall_pop <- rates_df %>%
  filter(age_group_label == "Overall") %>%
  select(jurisdiction, current_season, population) %>%
  rename(overall_population = population)

rates_df <- rates_df %>%
  left_join(overall_pop, by = c("jurisdiction", "current_season"))

rates_df <- rates_df %>%
  mutate(vax_rate = new_doses / coalesce(population, overall_population))

rates_df <- rates_df %>%
  mutate(start_year = as.integer(substr(current_season, 1, 4)),  # Extract the first year
         # Combine starting year with month to create a valid date
         date = as.Date(paste(start_year, month, "01", sep = "-"), format = "%Y-%b-%d"))


## plotting rates over time?? experimenting
temp_df <- rates_df %>%
  filter(age_group_label == "Overall") %>%
  group_by(date) %>%
  summarise(avg_rate = mean(new_doses, na.rm = TRUE))

test <- temp_df %>%
  ggplot(aes(x = date, y = avg_rate)) +
  geom_line()


# 22-23 and 23-24 have significantly different doses
season_22_23 <- rates_df |>
  filter(current_season == "2022-23")

season_23_24 <- rates_df |>
  filter(current_season == "2023-24")

test_1<-t.test(season_22_23$numerator,season_23_24$numerator)
test_1
