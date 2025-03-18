library(tidyverse)


file1 <- "ed_traj.csv"
file2 <- "ed_visits.csv"
df1 <- read.csv(file1)
df2 <- read.csv(file2)
# Convert week_end to Date format
df1$week_end <- as.Date(df1$week_end, format="%Y-%m-%d")
df2$week_end <- as.Date(df2$week_end, format="%Y-%m-%d")

# Clean df1 (Trajectories dataset) - Select relevant columns
df1_clean <- df1 %>%
  select(week_end, geography, county, percent_visits_influenza) %>%
  filter(!is.na(percent_visits_influenza))
# Clean df2 (Demographics dataset) - Select flu data only
df2_clean <- df2 %>%
  filter(pathogen == "Influenza") %>% # Select only Influenza-related ED visits
  select(week_end, geography, percent_visits) %>%
  rename(percent_visits_influenza = percent_visits) %>%
  filter(!is.na(percent_visits_influenza))

# Merge both datasets for better insights
df_combined <- bind_rows(df1_clean, df2_clean)

df_combined$Date <- as.Date(df_combined$week_end)

df_combined$year <- format(df_combined$week_end, "%Y")
df_combined$month <- format(df_combined$week_end, "%m")
df_combined$month_abbr <- month.abb[as.numeric(df_combined$month)]

seasonal <- df_combined %>%
  filter(county == "All") %>%
  group_by(Date) %>%
  summarise(percent_visits_influenza = mean(percent_visits_influenza))

seasonal$Date <- as.Date(seasonal$Date)
seasonal$year <- format(seasonal$Date, "%Y")
seasonal$month <- format(seasonal$Date, "%m")
seasonal$month_abbr <- month.abb[as.numeric(seasonal$month)]



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
  summarise(comb_doses = sum(vax_rate, na.rm = TRUE))

temp_df$month <- format(temp_df$date, "%m")


test <- temp_df %>%
  ggplot(aes(x = date, y = avg_rate)) +
  geom_line()

test

# 1. Deduplicate by keeping only the first row for each date, jurisdiction, age group, and season
df_dedup <- rates_df %>%
  group_by(jurisdiction, age_group_label, current_season, date) %>%
  slice(1) %>%               # Keep only the first row for each group
  ungroup()

# 2. Sort by jurisdiction, age group, season, and date to ensure proper order for calculating new doses
df_dedup <- df_dedup %>%
  arrange(jurisdiction, age_group_label, current_season, date)

# 3. Calculate new doses by comparing the cumulative totals
df_dedup <- df_dedup %>%
  group_by(jurisdiction, age_group_label, current_season) %>%
  mutate(new_doses = numerator - lag(numerator)) %>%    # Subtract previous month from current month
  ungroup()

df_for_cop <- df_dedup %>%
  group_by(date) %>%
  summarise(plot_col = sum(new_doses, na.rm = TRUE))


ggplot(seasonal, aes(x = factor(month_abbr), y = percent_visits_influenza, color = as.factor(year), group = year)) +
  geom_smooth()

ggplot() +
  geom_line(data=seasonal, aes(x = Date, y = percent_visits_influenza)) +
  geom_line(data=temp_df, aes(x = date, y = avg_rate))

merged = merge(df_for_cop, seasonal, by.x="date", by.y="Date")


cor.test(merged$plot_col, merged$percent_visits_influenza, method = "pearson")




# Create yearly summary table (limit to top 5 states)
summary_table <- df_combined %>%
  mutate(year = year(week_end)) %>%
  group_by(year, geography) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_percent_influenza)) %>%
  group_by(year) %>%
  slice_max(order_by = avg_percent_influenza, n = 5) # Keep only the top 5 states


## my work
df_for_plot <- df_combined %>%
  group_by(Date) %>%
  summarise(value = mean(percent_visits_influenza))

plot_time <- ggplot(data = df_for_plot) +
  geom_point(aes(x = Date, y = value))

plot_time



# Filter dataset for only top 5 states to avoid clutter in the bar plot
top_states <- unique(summary_table$geography)
df_filtered <- df_combined %>%
  mutate(year = year(week_end)) %>%
  filter(geography %in% top_states)
# Bar Plot: Yearly average flu-related ED visits for top 5 states
ggplot(summary_table, aes(x = as.factor(year), y = avg_percent_influenza, fill = geography)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(title = "Top 5 States with Highest Influenza-Related ED Visits (2022-2025)",
       x = "Year",
       y = "Average % of ED Visits",
       fill = "State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels for clarity

anova_result <- aov(percent_visits_influenza ~ as.factor(year), data = df_filtered)
summary(anova_result) # Print ANOVA test result

df_demo <- read.csv(file2)
df_demo_clean <- df_demo %>%
  filter(pathogen == "Influenza") %>% # Keep only Influenza-related ED visits
  select(week_end, demographics_type, demographics_values, percent_visits) %>%
  rename(percent_visits_influenza = percent_visits) %>%
  filter(!is.na(percent_visits_influenza)) # Remove missing values

top_age <- df_demo_clean %>%
  filter(demographics_type == "Age Group") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
top_race <- df_demo_clean %>%
  filter(demographics_type == "Race/Ethnicity") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
top_sex <- df_demo_clean %>%
  filter(demographics_type == "Sex") %>%
  group_by(demographics_values) %>%
  summarize(avg_percent_influenza = mean(percent_visits_influenza, na.rm = TRUE)) %>%
  arrange(desc(avg_percent_influenza))
# Print top demographic groups
print(top_age)


top_demo <- bind_rows(top_age, top_race, top_sex)
ggplot(top_demo, aes(x = demographics_values, y = avg_percent_influenza)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Highest Influenza-Related ED Visit Rate by Demographic Category from 2022-25",
       x = "Demographic Group (Age, Race, Sex)",
       y = "Average % of ED Visits",
       fill = "Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate labels for better readability


